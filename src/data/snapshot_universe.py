#!/usr/bin/env python3
"""Snapshot diario del universo de Binance (point-in-time).

Guarda cada día el estado de `exchangeInfo` y `ticker/24hr` de spot y de futuros
USDⓈ-M. Es el único dato del proyecto que NO se puede reconstruir hacia atrás:
la API solo responde por los símbolos vivos hoy, así que cada día sin snapshot
es un día de universo point-in-time perdido para siempre.

Usa solo la biblioteca estándar a propósito, para que corra desde el primer día
sin depender del entorno de uv ni de ninguna dependencia instalada.

Uso:
    python3 src/data/snapshot_universe.py [--out DIR] [--force]

Cron (dos veces al día; cron usa hora LOCAL, no UTC):
    5 9,21 * * * cd ~/Drive/Codigos/AlgoTrading && /usr/bin/python3 \
        src/data/snapshot_universe.py >> data/raw/snapshots/cron.log 2>&1

Son dos corridas porque el script es idempotente: la segunda no descarga nada
si la primera funcionó, y así un día con el equipo apagado a una de las dos
horas no se pierde. La carpeta destino siempre se nombra con la fecha UTC.
"""

from __future__ import annotations

import argparse
import gzip
import json
import sys
import time
import urllib.error
import urllib.request
from datetime import UTC, datetime
from pathlib import Path

USER_AGENT = "algotrading-snapshot/1.0"
TIMEOUT = 30
RETRIES = 3
RETRY_WAIT = 5

# nombre -> url. Spot y futuros USDⓈ-M.
ENDPOINTS = {
    "spot_exchange_info": "https://api.binance.com/api/v3/exchangeInfo",
    "spot_ticker_24hr": "https://api.binance.com/api/v3/ticker/24hr",
    "um_exchange_info": "https://fapi.binance.com/fapi/v1/exchangeInfo",
    "um_ticker_24hr": "https://fapi.binance.com/fapi/v1/ticker/24hr",
}

DEFAULT_OUT = Path("data/raw/snapshots")


def fetch(url: str) -> bytes:
    """Descarga una URL con reintentos. Devuelve el cuerpo crudo."""
    last_error: Exception | None = None
    for intento in range(1, RETRIES + 1):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
            with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
                if resp.status != 200:
                    raise urllib.error.HTTPError(url, resp.status, "status inesperado", resp.headers, None)
                return resp.read()
        except Exception as exc:  # noqa: BLE001 - se reintenta cualquier fallo de red
            last_error = exc
            if intento < RETRIES:
                time.sleep(RETRY_WAIT * intento)
    raise RuntimeError(f"no se pudo descargar {url}: {last_error}")


def resumen(nombre: str, payload: object) -> str:
    """Una línea legible para el log del cron, para detectar snapshots vacíos."""
    if isinstance(payload, dict) and "symbols" in payload:
        simbolos = payload["symbols"]
        vivos = sum(1 for s in simbolos if s.get("status") in ("TRADING", None))
        return f"{len(simbolos)} símbolos ({vivos} TRADING)"
    if isinstance(payload, list):
        return f"{len(payload)} registros"
    return "formato inesperado"


def guardar(destino: Path, crudo: bytes) -> int:
    """Escribe el JSON comprimido de forma atómica. Devuelve bytes escritos."""
    destino.parent.mkdir(parents=True, exist_ok=True)
    tmp = destino.with_suffix(destino.suffix + ".tmp")
    with gzip.open(tmp, "wb", compresslevel=6) as fh:
        fh.write(crudo)
    tmp.replace(destino)
    return destino.stat().st_size


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT, help=f"directorio destino (default: {DEFAULT_OUT})")
    parser.add_argument("--force", action="store_true", help="re-descargar aunque el snapshot de hoy ya exista")
    args = parser.parse_args()

    ahora = datetime.now(UTC)
    dia = ahora.strftime("%Y-%m-%d")
    carpeta = args.out / dia

    print(f"[{ahora.isoformat(timespec='seconds')}] snapshot {dia}")

    fallos = 0
    for nombre, url in ENDPOINTS.items():
        destino = carpeta / f"{nombre}.json.gz"
        if destino.exists() and not args.force:
            print(f"  = {nombre}: ya existe, se omite")
            continue
        try:
            crudo = fetch(url)
            payload = json.loads(crudo)  # valida que sea JSON antes de guardar
            escritos = guardar(destino, crudo)
            print(f"  + {nombre}: {resumen(nombre, payload)}, {escritos / 1024:.0f} KB -> {destino}")
        except Exception as exc:  # noqa: BLE001 - se reporta y se sigue con el resto
            fallos += 1
            print(f"  ! {nombre}: FALLO - {exc}", file=sys.stderr)

    # Marca de tiempo exacta de la corrida, para auditar huecos del cron.
    if fallos == 0:
        (carpeta / "_ok").write_text(ahora.isoformat(timespec="seconds") + "\n", encoding="utf-8")

    if fallos:
        print(f"  {fallos} de {len(ENDPOINTS)} endpoints fallaron", file=sys.stderr)
    return 1 if fallos else 0


if __name__ == "__main__":
    raise SystemExit(main())
