"""Tests del snapshot diario del universo.

No tocan la red: validan la configuración, el resumen y el guardado atómico,
que son las partes donde un error silencioso dejaría snapshots vacíos o
corruptos sin que el cron se entere.
"""

from __future__ import annotations

import gzip
import json
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from src.data.snapshot_universe import ENDPOINTS, guardar, resumen


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
    payload = {
        "symbols": [
            {"symbol": "BTCUSDT", "status": "TRADING"},
            {"symbol": "LUNAUSDT", "status": "BREAK"},
        ]
    }
    assert resumen("spot_exchange_info", payload) == "2 símbolos (1 TRADING)"


def test_resumen_cuenta_registros_de_ticker():
    assert resumen("spot_ticker_24hr", [{"symbol": "BTCUSDT"}, {"symbol": "ETHUSDT"}]) == "2 registros"


def test_resumen_no_revienta_con_formato_inesperado():
    """Binance puede devolver un error como dict: el cron debe seguir vivo."""
    assert resumen("spot_exchange_info", {"code": -1121, "msg": "Invalid symbol"}) == "formato inesperado"


def test_guardar_escribe_gzip_valido_y_sin_temporales(tmp_path):
    destino = tmp_path / "2026-01-01" / "spot_exchange_info.json.gz"
    original = {"symbols": [{"symbol": "BTCUSDT", "status": "TRADING"}]}

    escritos = guardar(destino, json.dumps(original).encode())

    assert escritos > 0
    assert leer_gz(destino) == original
    assert not list(destino.parent.glob("*.tmp")), "el temporal debe desaparecer al renombrar"


def test_guardar_sobrescribe_sin_dejar_restos(tmp_path):
    destino = tmp_path / "spot.json.gz"
    guardar(destino, json.dumps({"symbols": []}).encode())
    guardar(destino, json.dumps({"symbols": [{"symbol": "ETHUSDT", "status": "TRADING"}]}).encode())

    assert len(leer_gz(destino)["symbols"]) == 1


@pytest.mark.parametrize("nombre", sorted(ENDPOINTS))
def test_cada_endpoint_tiene_nombre_de_archivo_seguro(nombre):
    """Los nombres se usan como nombre de archivo: nada de separadores de ruta."""
    assert "/" not in nombre and ".." not in nombre
