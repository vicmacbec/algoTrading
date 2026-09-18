"""Universo point-in-time: qué pares existían y eran operables en cada fecha.

Sin esto, cualquier backtest elige su universo con los pares que siguen vivos
hoy, que es la definición del sesgo de supervivencia: desaparecen justo los que
habrían hecho perder dinero. BTCBUSD, LUNAUSDT o FTTUSDT no aparecen en la API
actual, pero sí operaron durante años.

Se combinan dos fuentes, cada una con su alcance:

- **El índice de `data.binance.vision`**, que llega hasta 2017. El primer mes con
  archivo de un símbolo marca su listado y el último su delisting. Es la única
  señal histórica disponible, con granularidad mensual.
- **Los snapshots diarios** (`snapshot_universe.py`), que desde 2026-09 dan el
  `status` exacto, los filtros de orden vigentes, el volumen del día y —lo más
  importante para este módulo— el `baseAsset` y el `quoteAsset` **autoritativos**.

El esqueleto lo da el bucket; los snapshots lo refinan a partir del día en que
se empezó a capturar.

## Por qué el nombre del par no basta

Los símbolos de Binance no llevan separador, así que partirlos es ambiguo y
ninguna regla simple acierta siempre. Contrastadas contra los 3705 símbolos de un
snapshot real, las variantes dan:

| Regla | Errores |
|---|---|
| Sufijo más largo | 10 (0.27 %) |
| Base conocida más larga | 7 (0.19 %) |
| **Base conocida + quote más largo** (la usada) | **4 (0.11 %)** |

Ejemplos de por qué: `ADAEUR` es `ADA`+`EUR` y no `AD`+`AEUR`; `BTCBUSD` es
`BTC`+`BUSD` y no `BTCB`+`USD`, aunque `BTCB` sea un activo real.

Los 4 errores que quedan son **ambigüedades irreducibles**: en `LUNAEUR` tanto
`LUNA`+`EUR` como `LUN`+`AEUR` parten en activos que existen, y el nombre no
contiene la información para decidir. Por eso el orden de preferencia es usar
siempre el mapa autoritativo del snapshot y caer en la heurística solo para
pares que desaparecieron antes de que hubiera snapshots.
"""

from __future__ import annotations

import gzip
import json
from collections.abc import Iterable
from dataclasses import dataclass
from datetime import date
from pathlib import Path

from src.data.binance_vision import archivos_de

# Quote assets observados en Binance. La lista importa poco cuando hay snapshot
# —de ahí sale la verdad— y mucho para los pares delistados hace años.
QUOTES_CONOCIDOS = (
    "USDSOLD", "FDUSD", "RLUSD", "PYUSD", "EURI", "BUSD", "TUSD", "USDP",
    "USDS", "AEUR", "BIDR", "IDRT", "BVND", "BKRW", "DOGE", "USDT", "USDC",
    "USD1", "DAI", "VAI", "UST", "BRL", "TRY", "UAH", "NGN", "RUB", "ZAR",
    "ARS", "JPY", "MXN", "PLN", "RON", "CZK", "COP", "IDR", "THB", "KZT",
    "AED", "EUR", "GBP", "AUD", "USD", "BNB", "BTC", "ETH", "XRP", "TRX",
    "SOL", "DOT", "PAX", "U",
)

# Pares entre dos de estos no aportan nada a una estrategia direccional: no se
# mueven. Contaminan el ranking de liquidez (USDCUSDT es el par más voluminoso
# del exchange) y hay que excluirlos del universo operable.
STABLECOINS = frozenset({
    "USDT", "USDC", "BUSD", "FDUSD", "TUSD", "USDP", "DAI", "USD1", "USDS",
    "UST", "USD", "EUR", "EURI", "AEUR", "GBP", "RLUSD", "PYUSD", "USDSOLD",
})


@dataclass(frozen=True)
class Cobertura:
    """Rango de meses con datos publicados para un símbolo."""

    simbolo: str
    primer_mes: str   # YYYY-MM
    ultimo_mes: str   # YYYY-MM
    meses: int
    quote: str = ""   # vacío = dedúcelo del nombre

    def __post_init__(self):
        if not self.quote:
            object.__setattr__(self, "quote", quote_de(self.simbolo))

    @property
    def base(self) -> str:
        return self.simbolo[: -len(self.quote)] if self.quote else self.simbolo

    def activo_en(self, momento: str) -> bool:
        """¿Había datos publicados de este símbolo en ese mes (YYYY-MM)?"""
        return self.primer_mes <= momento <= self.ultimo_mes

    def sigue_vivo(self, mes_actual: str) -> bool:
        """Un símbolo cuyo último archivo no es reciente está delistado.

        Se admite un mes de holgura porque el volcado del mes en curso puede
        tardar en publicarse.
        """
        return self.ultimo_mes >= mes_anterior(mes_actual)

    @property
    def es_par_estable(self) -> bool:
        return self.base in STABLECOINS and self.quote in STABLECOINS


def quote_de(simbolo: str, quotes: Iterable[str] = QUOTES_CONOCIDOS,
             bases_conocidas: Iterable[str] | None = None) -> str:
    """Deduce el quote asset del nombre, o '' si no se reconoce.

    Solo para pares sin snapshot. Entre las particiones posibles se prefiere el
    **quote más largo de entre las que dejan una base conocida**, que es la regla
    con menos errores medidos (4 de 3705, 0.11 %). Sin lista de bases se cae al
    sufijo más largo, que sube el error a 10.
    """
    candidatos = [q for q in quotes if simbolo.endswith(q) and len(simbolo) > len(q)]
    if not candidatos:
        return ""
    if bases_conocidas:
        conocidas = set(bases_conocidas)
        plausibles = [q for q in candidatos if simbolo[: -len(q)] in conocidas]
        if plausibles:
            return max(plausibles, key=len)
    return max(candidatos, key=len)


def mapa_assets(exchange_info: dict) -> tuple[dict[str, str], set[str]]:
    """Del snapshot saca la verdad: quote por símbolo y todas las bases vistas.

    Es lo que convierte la deducción en consulta para los ~3700 pares que
    existen hoy, y lo que alimenta la heurística para los que ya no.
    """
    quotes, bases = {}, set()
    for s in exchange_info.get("symbols", []):
        quotes[s["symbol"]] = s.get("quoteAsset", "")
        if s.get("baseAsset"):
            bases.add(s["baseAsset"])
    return quotes, bases


def mes_de(fecha: date | str) -> str:
    """Normaliza una fecha a YYYY-MM."""
    if isinstance(fecha, str):
        return fecha[:7]
    return f"{fecha.year:04d}-{fecha.month:02d}"


def mes_anterior(mes: str) -> str:
    anio, m = int(mes[:4]), int(mes[5:7])
    return f"{anio - 1:04d}-12" if m == 1 else f"{anio:04d}-{m - 1:02d}"


def meses_transcurridos(desde: str, hasta: str) -> int:
    a1, m1 = int(desde[:4]), int(desde[5:7])
    a2, m2 = int(hasta[:4]), int(hasta[5:7])
    return (a2 - a1) * 12 + (m2 - m1)


def cobertura_de(simbolo: str, intervalo: str = "1d", mercado: str = "spot",
                 quote: str = "") -> Cobertura | None:
    """Consulta al bucket qué meses tiene publicados un símbolo.

    Devuelve None si no hay ningún archivo, que es lo que ocurre con símbolos
    que existen en `exchangeInfo` pero nunca llegaron a operar. Pasa `quote`
    cuando lo tengas del snapshot: evita la heurística por completo.
    """
    archivos = archivos_de(simbolo, intervalo, mercado)
    periodos = sorted(a.periodo for a in archivos if a.periodo)
    if not periodos:
        return None
    return Cobertura(simbolo, periodos[0], periodos[-1], len(periodos), quote)


def elegibles_en(coberturas: Iterable[Cobertura], momento: date | str,
                 quote: str = "", meses_minimos: int = 0,
                 excluir_estables: bool = True) -> list[str]:
    """Los símbolos con datos publicados en esa fecha, filtrados.

    `meses_minimos` exige una antigüedad mínima *hasta esa fecha*, no en total:
    de otro modo se estaría usando información del futuro para decidir si un par
    era elegible en el pasado.
    """
    mes = mes_de(momento)
    salida = []
    for c in coberturas:
        if not c.activo_en(mes):
            continue
        if quote and c.quote != quote:
            continue
        if excluir_estables and c.es_par_estable:
            continue
        if meses_minimos and meses_transcurridos(c.primer_mes, mes) < meses_minimos:
            continue
        salida.append(c.simbolo)
    return sorted(salida)


# --- lectura de los snapshots diarios ---

def leer_snapshot(carpeta: Path, nombre: str = "spot_exchange_info") -> dict | list:
    """Carga un snapshot comprimido de un día."""
    with gzip.open(carpeta / f"{nombre}.json.gz", "rb") as fh:
        return json.load(fh)


def estado_por_simbolo(exchange_info: dict) -> dict[str, dict]:
    """Extrae del snapshot lo que define si un par es operable ese día.

    Se queda con el `status`, los assets y los tres filtros que deciden si una
    orden es válida. Esos filtros cambian con el tiempo, así que consultarlos
    hoy para simular 2023 daría tamaños de orden equivocados.
    """
    salida = {}
    for s in exchange_info.get("symbols", []):
        filtros = {f["filterType"]: f for f in s.get("filters", [])}
        salida[s["symbol"]] = {
            "status": s.get("status"),
            "base": s.get("baseAsset"),
            "quote": s.get("quoteAsset"),
            "tick_size": _num(filtros.get("PRICE_FILTER", {}).get("tickSize")),
            "step_size": _num(filtros.get("LOT_SIZE", {}).get("stepSize")),
            "min_notional": _num(filtros.get("NOTIONAL", {}).get("minNotional")
                                 or filtros.get("MIN_NOTIONAL", {}).get("minNotional")),
        }
    return salida


def volumen_por_simbolo(ticker: list) -> dict[str, float]:
    """Volumen en quote de las últimas 24 h, para ordenar por liquidez."""
    return {t["symbol"]: float(t.get("quoteVolume") or 0.0) for t in ticker}


def es_par_estable(base: str, quote: str) -> bool:
    """Un par entre dos stablecoins no se mueve: no sirve para operar dirección."""
    return base in STABLECOINS and quote in STABLECOINS


def top_por_liquidez(ticker: list, estados: dict[str, dict], n: int = 30,
                     quote: str = "USDT", excluir_estables: bool = True) -> list[str]:
    """Los N pares más líquidos que además estén operables ese día.

    Excluye por defecto los pares entre stablecoins: `USDCUSDT` es el símbolo
    con más volumen del exchange y no se mueve, así que encabezaría cualquier
    ranking sin aportar nada.
    """
    volumenes = volumen_por_simbolo(ticker)
    candidatos = []
    for simbolo, vol in volumenes.items():
        e = estados.get(simbolo)
        if not e or e.get("status") != "TRADING" or e.get("quote") != quote:
            continue
        if excluir_estables and es_par_estable(e.get("base", ""), e.get("quote", "")):
            continue
        candidatos.append((vol, simbolo))
    candidatos.sort(reverse=True)
    return [s for _, s in candidatos[:n]]


def _num(valor: str | None) -> float | None:
    return float(valor) if valor not in (None, "") else None
