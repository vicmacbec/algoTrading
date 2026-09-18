"""Tests del universo point-in-time.

Sin red: las coberturas y los snapshots se construyen a mano. Lo que se prueba
es la lógica que decide qué pares existían en una fecha, que es lo que separa un
backtest honesto de uno con sesgo de supervivencia.

Los casos de `quote_de` no son inventados: salen de contrastar la heurística
contra los 3705 símbolos reales de un snapshot, incluidos los cuatro que ninguna
regla basada en el nombre puede resolver.
"""

from __future__ import annotations

import gzip
import json
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from src.data.universe import (
    STABLECOINS,
    Cobertura,
    elegibles_en,
    es_par_estable,
    estado_por_simbolo,
    leer_snapshot,
    mapa_assets,
    mes_anterior,
    mes_de,
    meses_transcurridos,
    quote_de,
    top_por_liquidez,
    volumen_por_simbolo,
)

# Coberturas reales observadas en el bucket.
BTCUSDT = Cobertura("BTCUSDT", "2017-08", "2026-08", 109)
BTCBUSD = Cobertura("BTCBUSD", "2019-09", "2023-12", 52)   # delistado
LUNAUSDT = Cobertura("LUNAUSDT", "2020-08", "2026-08", 73)

# Bases reales del snapshot que hacen ambiguas las particiones.
BASES_REALES = {"ADA", "LUNA", "LUN", "THETA", "GALA", "GAL", "ENA", "USDT",
                "ARB", "AR", "BNB", "BTC", "BTCB", "ETH", "BUSD", "SOL", "U",
                "USDC", "TUSD", "QNT", "XRP"}


# --- partir el nombre del par: donde la intuición falla ---

@pytest.mark.parametrize(("simbolo", "esperado"), [
    ("BTCUSDT", "USDT"),
    ("ETHBTC", "BTC"),
    ("BNBFDUSD", "FDUSD"),
    ("ADABUSD", "BUSD"),
    ("SOLBNB", "BNB"),
])
def test_quote_en_los_casos_sin_ambiguedad(simbolo, esperado):
    assert quote_de(simbolo, bases_conocidas=BASES_REALES) == esperado


@pytest.mark.parametrize(("simbolo", "esperado"), [
    ("ADAEUR", "EUR"),      # no AEUR: AD no es un activo, ADA sí
    ("THETAEUR", "EUR"),
    ("ENAEUR", "EUR"),
    ("USDTUSD", "USD"),     # no TUSD
    ("BUSDUSDT", "USDT"),   # BUSD contra USDT, no BUS contra DUSDT
    ("BTCBUSD", "BUSD"),    # no BTCB + USD, aunque BTCB exista como activo
    ("TUSDBUSD", "BUSD"),
    ("QNTBUSD", "BUSD"),
])
def test_quote_resuelve_las_ambiguedades_reales(simbolo, esperado):
    """Casos donde el sufijo más largo o la base más larga daban mal."""
    assert quote_de(simbolo, bases_conocidas=BASES_REALES) == esperado


@pytest.mark.parametrize("simbolo", ["LUNAEUR", "GALAEUR", "ARBIDR"])
def test_ambiguedades_irreducibles_estan_documentadas(simbolo):
    """El nombre no alcanza: ambas particiones dan activos que existen.

    `LUNAEUR` puede ser LUNA+EUR o LUN+AEUR, y las dos bases son reales. La
    heurística falla en 4 de 3705 símbolos (0.11 %) por esta razón, y por eso
    el mapa autoritativo del snapshot tiene siempre prioridad sobre ella.
    """
    deducido = quote_de(simbolo, bases_conocidas=BASES_REALES)
    assert deducido != "", "debe devolver algo: el fallo es de precisión, no de cobertura"


def test_sin_bases_conocidas_el_error_sube():
    """Degradación documentada: sin la lista de bases, ADAEUR se parte mal."""
    assert quote_de("ADAEUR") == "AEUR"
    assert quote_de("ADAEUR", bases_conocidas=BASES_REALES) == "EUR"


def test_quote_desconocido_devuelve_vacio():
    """Preferible saber que no se sabe antes que adivinar."""
    assert quote_de("ALGOXYZ") == ""


def test_base_se_obtiene_quitando_el_quote():
    assert BTCUSDT.base == "BTC"
    assert BTCUSDT.quote == "USDT"


def test_cobertura_acepta_el_quote_autoritativo():
    """Cuando hay snapshot no se deduce nada: se usa el dato del exchange."""
    c = Cobertura("ADAEUR", "2021-01", "2023-01", 24, quote="EUR")
    assert c.quote == "EUR" and c.base == "ADA"


def test_mapa_assets_saca_la_verdad_del_snapshot():
    quotes, bases = mapa_assets(snapshot_falso())
    assert quotes["BTCUSDT"] == "USDT"
    assert "BTC" in bases and "ETH" in bases


# --- pares entre stablecoins ---

def test_par_entre_stablecoins_se_reconoce():
    assert es_par_estable("USDC", "USDT")
    assert es_par_estable("FDUSD", "USDT")
    assert not es_par_estable("BTC", "USDT")


def test_stablecoins_incluye_las_del_top_real():
    """USDC, USD1, FDUSD y RLUSD encabezaban el ranking de volumen."""
    for s in ("USDC", "USD1", "FDUSD", "RLUSD"):
        assert s in STABLECOINS


def test_elegibles_excluye_pares_estables_por_defecto():
    estable = Cobertura("USDCUSDT", "2018-12", "2026-08", 88)
    assert elegibles_en([estable, BTCUSDT], "2021-06") == ["BTCUSDT"]
    assert "USDCUSDT" in elegibles_en([estable, BTCUSDT], "2021-06", excluir_estables=False)


# --- aritmética de meses ---

def test_mes_de_acepta_fecha_y_texto():
    from datetime import date
    assert mes_de(date(2026, 9, 18)) == "2026-09"
    assert mes_de("2026-09-18") == "2026-09"


def test_mes_anterior_cruza_el_año():
    assert mes_anterior("2026-01") == "2025-12"
    assert mes_anterior("2026-09") == "2026-08"


def test_meses_transcurridos():
    assert meses_transcurridos("2017-08", "2026-08") == 108
    assert meses_transcurridos("2026-09", "2026-09") == 0


# --- lo esencial: qué existía en cada fecha ---

def test_un_par_delistado_estaba_vivo_en_su_momento():
    """El punto de todo el módulo: BTCBUSD no existe hoy pero operó 52 meses."""
    assert BTCBUSD.activo_en("2021-06")
    assert not BTCBUSD.activo_en("2026-09")


def test_elegibles_excluye_los_que_aun_no_existian():
    assert elegibles_en([BTCUSDT, BTCBUSD, LUNAUSDT], "2018-01") == ["BTCUSDT"]


def test_elegibles_incluye_los_delistados_en_su_epoca():
    assert elegibles_en([BTCUSDT, BTCBUSD, LUNAUSDT], "2021-06") == [
        "BTCBUSD", "BTCUSDT", "LUNAUSDT"]


def test_elegibles_filtra_por_quote():
    coberturas = [BTCUSDT, BTCBUSD, LUNAUSDT]
    assert elegibles_en(coberturas, "2021-06", quote="USDT") == ["BTCUSDT", "LUNAUSDT"]
    assert elegibles_en(coberturas, "2021-06", quote="BUSD") == ["BTCBUSD"]


def test_antiguedad_minima_se_mide_hasta_la_fecha_no_en_total():
    """Exigir antigüedad con el total sería usar información del futuro."""
    assert elegibles_en([BTCBUSD], "2019-09", meses_minimos=6) == []
    assert elegibles_en([BTCBUSD], "2020-06", meses_minimos=6) == ["BTCBUSD"]


def test_sigue_vivo_admite_un_mes_de_holgura():
    """El volcado del mes en curso tarda en publicarse."""
    assert BTCUSDT.sigue_vivo("2026-09")
    assert not BTCBUSD.sigue_vivo("2026-09")


# --- lectura de los snapshots diarios ---

def snapshot_falso() -> dict:
    return {"symbols": [
        {"symbol": "BTCUSDT", "status": "TRADING", "baseAsset": "BTC", "quoteAsset": "USDT",
         "filters": [{"filterType": "PRICE_FILTER", "tickSize": "0.01000000"},
                     {"filterType": "LOT_SIZE", "stepSize": "0.00001000"},
                     {"filterType": "NOTIONAL", "minNotional": "5.00000000"}]},
        {"symbol": "OLDCOINUSDT", "status": "BREAK", "baseAsset": "OLDCOIN", "quoteAsset": "USDT",
         "filters": []},
        {"symbol": "ETHBTC", "status": "TRADING", "baseAsset": "ETH", "quoteAsset": "BTC",
         "filters": []},
        {"symbol": "USDCUSDT", "status": "TRADING", "baseAsset": "USDC", "quoteAsset": "USDT",
         "filters": []},
    ]}


def test_estado_extrae_los_filtros_que_validan_una_orden():
    e = estado_por_simbolo(snapshot_falso())["BTCUSDT"]
    assert e["status"] == "TRADING"
    assert e["tick_size"] == 0.01
    assert e["step_size"] == 0.00001
    assert e["min_notional"] == 5.0


def test_estado_tolera_simbolos_sin_filtros():
    e = estado_por_simbolo(snapshot_falso())["OLDCOINUSDT"]
    assert e["status"] == "BREAK"
    assert e["min_notional"] is None


def test_top_por_liquidez_excluye_lo_no_operable_y_lo_estable():
    ticker = [
        {"symbol": "USDCUSDT", "quoteVolume": "2762152552"},   # el más líquido, pero estable
        {"symbol": "BTCUSDT", "quoteVolume": "840889265"},
        {"symbol": "OLDCOINUSDT", "quoteVolume": "9999999"},   # volumen alto pero en BREAK
        {"symbol": "ETHBTC", "quoteVolume": "5000000"},        # operable pero quote BTC
    ]
    estados = estado_por_simbolo(snapshot_falso())
    assert top_por_liquidez(ticker, estados, n=10, quote="USDT") == ["BTCUSDT"]
    assert top_por_liquidez(ticker, estados, n=10, quote="USDT",
                            excluir_estables=False)[0] == "USDCUSDT"


def test_volumen_por_simbolo_tolera_campos_vacios():
    assert volumen_por_simbolo([{"symbol": "X", "quoteVolume": None}]) == {"X": 0.0}


def test_leer_snapshot_descomprime(tmp_path):
    datos = snapshot_falso()
    with gzip.open(tmp_path / "spot_exchange_info.json.gz", "wb") as fh:
        fh.write(json.dumps(datos).encode())
    assert leer_snapshot(tmp_path) == datos
