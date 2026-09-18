"""Tests del snapshot diario del universo.

No tocan la red ni AWS: validan la configuración, el parseo del destino, el
resumen y el guardado atómico, que son las partes donde un error silencioso
dejaría snapshots vacíos, mal ubicados o corruptos sin que nadie se entere.
"""

from __future__ import annotations

import gzip
import json
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from src.data.snapshot_universe import (
    ENDPOINTS,
    Destino,
    clave,
    escribir,
    parse_destino,
    resumen,
)


def leer_gz(ruta: Path):
    """Lee un JSON comprimido cerrando siempre el archivo."""
    with gzip.open(ruta, "rb") as fh:
        return json.load(fh)


def test_endpoints_cubren_spot_y_futuros():
    """Si alguien borra un endpoint, el universo point-in-time queda incompleto."""
    assert set(ENDPOINTS) == {
        "spot_exchange_info",
        "spot_ticker_24hr",
        "um_exchange_info",
        "um_ticker_24hr",
    }
    for url in ENDPOINTS.values():
        assert url.startswith("https://"), "nunca http plano contra el exchange"


def test_resumen_cuenta_simbolos_de_exchange_info():
    payload = {"symbols": [{"symbol": "BTCUSDT", "status": "TRADING"},
                           {"symbol": "LUNAUSDT", "status": "BREAK"}]}
    assert resumen("spot_exchange_info", payload) == "2 símbolos (1 TRADING)"


def test_resumen_cuenta_registros_de_ticker():
    assert resumen("spot_ticker_24hr", [{"symbol": "BTCUSDT"}, {"symbol": "ETHUSDT"}]) == "2 registros"


def test_resumen_no_revienta_con_formato_inesperado():
    """Binance puede devolver un error como dict: el proceso debe seguir vivo."""
    assert resumen("spot_exchange_info", {"code": -1121, "msg": "Invalid symbol"}) == "formato inesperado"


# --- destino local vs S3 ---

def test_parse_destino_local():
    d = parse_destino("data/raw/snapshots")
    assert d.tipo == "local" and d.raiz == "data/raw/snapshots"


def test_parse_destino_s3_con_prefijo():
    d = parse_destino("s3://algotrading-vicmacbec-data/snapshots")
    assert d.tipo == "s3"
    assert d.bucket == "algotrading-vicmacbec-data"
    assert d.prefijo == "snapshots"


def test_parse_destino_s3_sin_prefijo():
    d = parse_destino("s3://mi-bucket")
    assert d.bucket == "mi-bucket" and d.prefijo == ""


def test_parse_destino_s3_tolera_barra_final():
    assert parse_destino("s3://mi-bucket/snapshots/").prefijo == "snapshots"


def test_parse_destino_s3_sin_bucket_es_error():
    with pytest.raises(ValueError):
        parse_destino("s3://")


def test_clave_s3_incluye_el_prefijo():
    d = parse_destino("s3://b/snapshots")
    assert clave(d, "2026-01-02", "spot_exchange_info.json.gz") == "snapshots/2026-01-02/spot_exchange_info.json.gz"


def test_clave_local_no_incluye_la_raiz():
    """En local la raíz se aplica al escribir, no se duplica en la clave."""
    d = parse_destino("data/raw/snapshots")
    assert clave(d, "2026-01-02", "_ok") == "2026-01-02/_ok"


def test_clave_s3_sin_prefijo_queda_en_la_raiz_del_bucket():
    assert clave(parse_destino("s3://b"), "2026-01-02", "_ok") == "2026-01-02/_ok"


# --- escritura local ---

def test_escribir_local_comprime_y_no_deja_temporales(tmp_path):
    d = Destino("local", str(tmp_path))
    original = {"symbols": [{"symbol": "BTCUSDT", "status": "TRADING"}]}

    escritos = escribir(d, "2026-01-01/spot_exchange_info.json.gz", json.dumps(original).encode())

    destino = tmp_path / "2026-01-01" / "spot_exchange_info.json.gz"
    assert escritos > 0
    assert leer_gz(destino) == original
    assert not list(destino.parent.glob("*.tmp")), "el temporal debe desaparecer al renombrar"


def test_escribir_local_sin_comprimir_para_la_marca_ok(tmp_path):
    d = Destino("local", str(tmp_path))
    escribir(d, "2026-01-01/_ok", b"2026-01-01T00:05:00+00:00", comprimir=False)
    assert (tmp_path / "2026-01-01" / "_ok").read_text().startswith("2026-01-01T")


def test_escribir_local_sobrescribe_sin_dejar_restos(tmp_path):
    d = Destino("local", str(tmp_path))
    escribir(d, "spot.json.gz", json.dumps({"symbols": []}).encode())
    escribir(d, "spot.json.gz", json.dumps({"symbols": [{"symbol": "ETHUSDT"}]}).encode())
    assert len(leer_gz(tmp_path / "spot.json.gz")["symbols"]) == 1


@pytest.mark.parametrize("nombre", sorted(ENDPOINTS))
def test_cada_endpoint_tiene_nombre_de_archivo_seguro(nombre):
    """Los nombres se usan como ruta de archivo/objeto: nada de separadores."""
    assert "/" not in nombre and ".." not in nombre
