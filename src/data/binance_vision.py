"""Acceso a los volcados públicos de `data.binance.vision`.

Binance publica su histórico completo como archivos ZIP en un bucket de S3
abierto, sin autenticación y sin límites de rate. Es la única forma sensata de
bajar años de velas: la API daría cientos de miles de peticiones.

Dos trampas que este módulo resuelve y que corrompen los datos en silencio si
se ignoran:

1. **Las unidades de tiempo cambiaron a mitad del histórico.** Los archivos
   hasta 2024 traen milisegundos (13 dígitos) y los de 2025 en adelante
   microsegundos (16 dígitos). Asumir una sola unidad manda la mitad del panel
   al año 57000. Se detecta por magnitud, valor por valor.
2. **La cabecera depende del mercado.** Los CSV de spot no la traen en ninguna
   época; los de futuros sí. Se olfatea: si el primer campo no es un entero, esa
   fila es cabecera.

El listado del bucket sirve además para reconstruir el universo point-in-time:
el primer mes con archivo de un símbolo es su listado y el último su delisting,
que es información que la API ya no devuelve.
"""

from __future__ import annotations

import hashlib
import io
import urllib.error
import urllib.parse
import urllib.request
import zipfile
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path

BASE_DESCARGA = "https://data.binance.vision"
BASE_LISTADO = "https://s3-ap-northeast-1.amazonaws.com/data.binance.vision"
ESPACIO_S3 = "{http://s3.amazonaws.com/doc/2006-03-01/}"

USER_AGENT = "algotrading-ingesta/1.0"
TIMEOUT = 120
REINTENTOS = 3

# Los 12 campos de una vela, en orden. Los CSV de spot no traen cabecera, así
# que el orden es el contrato. `ignore` se descarta al normalizar.
COLUMNAS_KLINE = (
    "open_time", "open", "high", "low", "close", "volume", "close_time",
    "quote_volume", "trades", "taker_buy_base", "taker_buy_quote", "ignore",
)
NUMERICAS_KLINE = ("open", "high", "low", "close", "volume", "quote_volume",
                   "taker_buy_base", "taker_buy_quote")


@dataclass(frozen=True)
class Archivo:
    """Un objeto del bucket: su clave, tamaño y la URL para descargarlo."""

    clave: str
    tamano: int

    @property
    def url(self) -> str:
        return f"{BASE_DESCARGA}/{self.clave}"

    @property
    def nombre(self) -> str:
        return self.clave.rsplit("/", 1)[-1]

    @property
    def periodo(self) -> str:
        """El YYYY-MM o YYYY-MM-DD que lleva el nombre, o '' si no lo tiene.

        Los nombres mezclan guiones del propio símbolo y del intervalo
        (`BTCUSDT-4h-2025-06.zip`), así que se prueba primero la forma diaria
        y luego la mensual, exigiendo que el año tenga cuatro dígitos.
        """
        partes = self.nombre.removesuffix(".zip").split("-")
        for n in (3, 2):  # YYYY-MM-DD antes que YYYY-MM
            if len(partes) >= n:
                candidato = partes[-n:]
                if len(candidato[0]) == 4 and all(t.isdigit() for t in candidato):
                    return "-".join(candidato)
        return ""


def _pedir(url: str) -> bytes:
    ultimo: Exception | None = None
    for intento in range(1, REINTENTOS + 1):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
            with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
                return r.read()
        except Exception as exc:  # noqa: BLE001 - la red falla de muchas formas
            ultimo = exc
            if intento == REINTENTOS:
                break
    raise RuntimeError(f"no se pudo obtener {url}: {ultimo}")


def _listar_pagina(prefijo: str, delimitador: str, marcador: str) -> bytes:
    params = {"prefix": prefijo, "delimiter": delimitador}
    if marcador:
        params["marker"] = marcador
    return _pedir(f"{BASE_LISTADO}?{urllib.parse.urlencode(params)}")


def listar_objetos(prefijo: str) -> Iterator[Archivo]:
    """Recorre todos los objetos bajo un prefijo, paginando con NextMarker."""
    import xml.etree.ElementTree as ET

    marcador = ""
    while True:
        raiz = ET.fromstring(_listar_pagina(prefijo, "", marcador))
        ultima = ""
        for nodo in raiz.findall(f"{ESPACIO_S3}Contents"):
            clave = nodo.findtext(f"{ESPACIO_S3}Key", "")
            tamano = int(nodo.findtext(f"{ESPACIO_S3}Size", "0"))
            ultima = clave
            yield Archivo(clave, tamano)
        if raiz.findtext(f"{ESPACIO_S3}IsTruncated", "false") != "true":
            return
        # El bucket no siempre devuelve NextMarker cuando no hay delimitador:
        # en ese caso se continúa desde la última clave vista.
        marcador = raiz.findtext(f"{ESPACIO_S3}NextMarker", "") or ultima
        if not marcador:
            return


def listar_prefijos(prefijo: str) -> Iterator[str]:
    """Devuelve los nombres de un nivel del árbol (símbolos, intervalos...)."""
    import xml.etree.ElementTree as ET

    marcador = ""
    while True:
        raiz = ET.fromstring(_listar_pagina(prefijo, "/", marcador))
        ultimo = ""
        for nodo in raiz.findall(f"{ESPACIO_S3}CommonPrefixes"):
            completo = nodo.findtext(f"{ESPACIO_S3}Prefix", "")
            ultimo = completo
            yield completo.removeprefix(prefijo).rstrip("/")
        if raiz.findtext(f"{ESPACIO_S3}IsTruncated", "false") != "true":
            return
        marcador = raiz.findtext(f"{ESPACIO_S3}NextMarker", "") or ultimo
        if not marcador:
            return


def prefijo_klines(mercado: str, periodicidad: str, simbolo: str = "", intervalo: str = "") -> str:
    """Construye el prefijo del bucket. mercado: 'spot' | 'futures/um'."""
    partes = ["data", mercado, periodicidad, "klines"]
    if simbolo:
        partes.append(simbolo)
        if intervalo:
            partes.append(intervalo)
    return "/".join(partes) + "/"


def simbolos(mercado: str = "spot", periodicidad: str = "monthly") -> list[str]:
    """Todos los símbolos con histórico publicado, incluidos los delistados."""
    return sorted(listar_prefijos(prefijo_klines(mercado, periodicidad)))


def archivos_de(simbolo: str, intervalo: str, mercado: str = "spot",
                periodicidad: str = "monthly") -> list[Archivo]:
    """Los ZIP disponibles de un símbolo, sin los .CHECKSUM."""
    prefijo = prefijo_klines(mercado, periodicidad, simbolo, intervalo)
    return [a for a in listar_objetos(prefijo) if a.clave.endswith(".zip")]


def descargar(archivo: Archivo, destino: Path, verificar: bool = True) -> Path:
    """Descarga un ZIP y comprueba su SHA-256. Si ya existe y cuadra, no repite."""
    destino.parent.mkdir(parents=True, exist_ok=True)
    if destino.exists() and destino.stat().st_size == archivo.tamano:
        return destino

    crudo = _pedir(archivo.url)
    if verificar:
        esperado = _pedir(archivo.url + ".CHECKSUM").decode().split()[0]
        real = hashlib.sha256(crudo).hexdigest()
        if real != esperado:
            raise RuntimeError(f"checksum no coincide en {archivo.nombre}: {real} != {esperado}")

    tmp = destino.with_suffix(destino.suffix + ".tmp")
    tmp.write_bytes(crudo)
    tmp.replace(destino)
    return destino


def escala_a_ms(valor: int) -> int:
    """Normaliza un timestamp a milisegundos, venga en s, ms, µs o ns.

    Binance cambió de milisegundos a microsegundos en 2025 sin avisar y sin
    cambiar el formato del archivo, así que la unidad se deduce de la magnitud.
    Referencia: 2001-09-09 son 1e12 ms; ningún timestamp real del proyecto cae
    fuera de estos rangos.
    """
    if valor < 1_000_000_000_0:          # < 1e10 -> segundos
        return valor * 1000
    if valor < 1_000_000_000_000_0:      # < 1e13 -> milisegundos
        return valor
    if valor < 1_000_000_000_000_000_0:  # < 1e16 -> microsegundos
        return valor // 1000
    return valor // 1_000_000            # nanosegundos


def parsear_csv(crudo: bytes, columnas: tuple[str, ...] | None = None) -> list[dict]:
    """Convierte el CSV de un volcado en filas, tolerando cabecera o su ausencia.

    Si `columnas` es None se toman las de la cabecera. Si el archivo no la trae
    (todo el spot), hay que pasarlas: el orden es el contrato.
    """
    texto = crudo.decode("utf-8", errors="replace").strip()
    if not texto:
        return []

    lineas = texto.splitlines()
    primera = lineas[0].split(",")
    tiene_cabecera = not primera[0].strip().lstrip("-").isdigit()

    if tiene_cabecera:
        nombres = tuple(c.strip() for c in primera)
        cuerpo = lineas[1:]
    else:
        if columnas is None:
            raise ValueError("el archivo no trae cabecera y no se indicaron columnas")
        nombres = columnas
        cuerpo = lineas

    filas = []
    for linea in cuerpo:
        if not linea.strip():
            continue
        valores = linea.split(",")
        if len(valores) != len(nombres):
            raise ValueError(f"esperaba {len(nombres)} columnas y llegaron {len(valores)}")
        filas.append(dict(zip(nombres, valores, strict=True)))
    return filas


def leer_klines(crudo_zip: bytes) -> list[dict]:
    """Extrae las velas de un ZIP y las normaliza: tiempos en ms, números float."""
    with zipfile.ZipFile(io.BytesIO(crudo_zip)) as z:
        nombre = z.namelist()[0]
        crudo = z.read(nombre)

    filas = parsear_csv(crudo, COLUMNAS_KLINE)
    salida = []
    for f in filas:
        vela = {
            "open_time": escala_a_ms(int(f["open_time"])),
            "close_time": escala_a_ms(int(f["close_time"])),
            "trades": int(float(f.get("trades") or f.get("count") or 0)),
        }
        for c in NUMERICAS_KLINE:
            origen = f.get(c)
            if origen is None:  # los futuros nombran distinto algunas columnas
                origen = f.get({"taker_buy_base": "taker_buy_volume",
                                "taker_buy_quote": "taker_buy_quote_volume"}.get(c, c))
            vela[c] = float(origen)
        salida.append(vela)
    return salida
