"""Tests del acceso a los volcados de data.binance.vision.

No tocan la red: todo se ejercita con bytes sintéticos. Lo que se prueba aquí
son las dos trampas que corrompen el panel en silencio —el cambio de unidades
de tiempo a mitad del histórico y la cabecera que solo traen los futuros— más
el parseo de nombres, que decide de qué mes es cada archivo.
"""

from __future__ import annotations

import io
import sys
import zipfile
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from src.data.binance_vision import (
    COLUMNAS_KLINE,
    Archivo,
    escala_a_ms,
    leer_klines,
    parsear_csv,
    prefijo_klines,
)

# Valores reales observados en el bucket, no inventados.
MS_2017 = 1502942400000       # BTCUSDT-4h-2017-08, milisegundos
US_2025 = 1748736000000000    # BTCUSDT-4h-2025-06, microsegundos


def zip_con(texto: str, nombre: str = "datos.csv") -> bytes:
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr(nombre, texto)
    return buf.getvalue()


# --- unidades de tiempo: la trampa principal ---

def test_escala_detecta_milisegundos_de_2017():
    assert escala_a_ms(MS_2017) == MS_2017


def test_escala_convierte_microsegundos_de_2025():
    assert escala_a_ms(US_2025) == US_2025 // 1000


def test_ambas_epocas_caen_en_fechas_plausibles():
    """Si la detección falla, una de las dos se va al año 57000."""
    from datetime import UTC, datetime

    a = datetime.fromtimestamp(escala_a_ms(MS_2017) / 1000, UTC)
    b = datetime.fromtimestamp(escala_a_ms(US_2025) / 1000, UTC)
    assert a.year == 2017
    assert b.year == 2025


@pytest.mark.parametrize(("entrada", "esperado"), [
    (1_502_942_400, 1_502_942_400_000),              # segundos
    (1_502_942_400_000, 1_502_942_400_000),          # milisegundos
    (1_502_942_400_000_000, 1_502_942_400_000),      # microsegundos
    (1_502_942_400_000_000_000, 1_502_942_400_000),  # nanosegundos
])
def test_escala_normaliza_cualquier_unidad(entrada, esperado):
    assert escala_a_ms(entrada) == esperado


# --- cabecera presente o ausente ---

def test_parsea_spot_sin_cabecera():
    linea = ",".join([str(MS_2017)] + ["1.0"] * 5 + [str(MS_2017 + 1)] + ["2.0"] * 5)
    filas = parsear_csv(linea.encode(), COLUMNAS_KLINE)
    assert len(filas) == 1
    assert filas[0]["open_time"] == str(MS_2017)


def test_parsea_futuros_con_cabecera():
    crudo = b"calc_time,funding_interval_hours,last_funding_rate\n1748736000000,8,0.0001\n"
    filas = parsear_csv(crudo)
    assert filas == [{"calc_time": "1748736000000", "funding_interval_hours": "8",
                      "last_funding_rate": "0.0001"}]


def test_sin_cabecera_y_sin_columnas_es_error():
    """Adivinar el orden de las columnas sería peor que fallar."""
    with pytest.raises(ValueError):
        parsear_csv(b"1748736000000,1,2,3\n")


def test_numero_de_columnas_inesperado_es_error():
    with pytest.raises(ValueError, match="columnas"):
        parsear_csv(b"1,2,3\n", COLUMNAS_KLINE)


def test_csv_vacio_devuelve_lista_vacia():
    assert parsear_csv(b"", COLUMNAS_KLINE) == []


# --- lectura completa de un ZIP ---

def vela(ts: int) -> str:
    return ",".join([str(ts), "100.5", "101.0", "99.5", "100.0", "12.5",
                     str(ts + 1), "1250.0", "42", "6.0", "600.0", "0"])


def test_leer_klines_normaliza_las_dos_epocas():
    for ts, esperado in ((MS_2017, MS_2017), (US_2025, US_2025 // 1000)):
        filas = leer_klines(zip_con(vela(ts)))
        assert len(filas) == 1
        assert filas[0]["open_time"] == esperado
        assert filas[0]["close"] == 100.0
        assert filas[0]["trades"] == 42
        assert filas[0]["taker_buy_quote"] == 600.0


def test_leer_klines_descarta_la_columna_ignore():
    assert "ignore" not in leer_klines(zip_con(vela(MS_2017)))[0]


def test_leer_klines_acepta_los_nombres_de_futuros():
    """Los futuros llaman taker_buy_volume a lo que en spot es taker_buy_base."""
    cabecera = ("open_time,open,high,low,close,volume,close_time,quote_volume,"
                "count,taker_buy_volume,taker_buy_quote_volume,ignore")
    filas = leer_klines(zip_con(cabecera + "\n" + vela(US_2025)))
    assert filas[0]["taker_buy_base"] == 6.0
    assert filas[0]["trades"] == 42


# --- nombres y prefijos ---

@pytest.mark.parametrize(("nombre", "esperado"), [
    ("BTCUSDT-4h-2025-06.zip", "2025-06"),
    ("BTCUSDT-1d-2017-08.zip", "2017-08"),
    ("BTCUSDT-metrics-2025-06-02.zip", "2025-06-02"),
    ("BTCUSDT-fundingRate-2025-06.zip", "2025-06"),
])
def test_periodo_sale_del_nombre(nombre, esperado):
    assert Archivo(f"data/spot/monthly/klines/BTCUSDT/4h/{nombre}", 0).periodo == esperado


def test_url_y_nombre_del_archivo():
    a = Archivo("data/spot/monthly/klines/BTCUSDT/4h/BTCUSDT-4h-2025-06.zip", 11528)
    assert a.url.startswith("https://data.binance.vision/data/spot/")
    assert a.nombre == "BTCUSDT-4h-2025-06.zip"


def test_prefijos_de_spot_y_futuros():
    assert prefijo_klines("spot", "monthly") == "data/spot/monthly/klines/"
    assert (prefijo_klines("futures/um", "monthly", "BTCUSDT", "4h")
            == "data/futures/um/monthly/klines/BTCUSDT/4h/")
