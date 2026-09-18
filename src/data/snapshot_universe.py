#!/usr/bin/env python3
"""Snapshot diario del universo de Binance (point-in-time).

Guarda cada día el estado de `exchangeInfo` y `ticker/24hr` de spot y de futuros
USDⓈ-M. Es el único dato del proyecto que NO se puede reconstruir hacia atrás:
la API solo responde por los símbolos vivos hoy, así que cada día sin snapshot
es un día de universo point-in-time perdido para siempre.

El destino puede ser local o S3. El núcleo usa solo la biblioteca estándar a
propósito, para que corra aunque el entorno falle; `boto3` se importa de forma
perezosa y únicamente cuando el destino es `s3://` (en Lambda ya viene incluido).

Uso local:
    python3 src/data/snapshot_universe.py
    python3 src/data/snapshot_universe.py --out s3://algotrading-vicmacbec-data/snapshots

En Lambda: handler `snapshot_universe.lambda_handler`, con la variable de entorno
    SNAPSHOT_OUT=s3://algotrading-vicmacbec-data/snapshots

Cron local de respaldo (cron usa hora LOCAL, no UTC):
    5 9,21 * * * cd ~/Drive/Codigos/AlgoTrading && /usr/bin/python3 \
        src/data/snapshot_universe.py >> data/raw/snapshots/cron.log 2>&1

Son dos corridas porque el script es idempotente: la segunda no descarga nada
si la primera funcionó. La carpeta destino siempre se nombra con la fecha UTC.
"""

from __future__ import annotations

import argparse
import gzip
import json
import os
import sys
import time
import urllib.error
import urllib.request
from dataclasses import dataclass
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

DEFAULT_OUT = "data/raw/snapshots"


@dataclass(frozen=True)
class Destino:
    """Dónde se escriben los snapshots: disco local o un prefijo de S3."""

    tipo: str  # "local" | "s3"
    raiz: str  # ruta local, o "bucket/prefijo"

    @property
    def bucket(self) -> str:
        return self.raiz.split("/", 1)[0]

    @property
    def prefijo(self) -> str:
        partes = self.raiz.split("/", 1)
        return partes[1].strip("/") if len(partes) > 1 else ""


def parse_destino(valor: str) -> Destino:
    """Convierte '--out' en un Destino. Acepta ruta local o s3://bucket/prefijo."""
    if valor.startswith("s3://"):
        raiz = valor[len("s3://"):].strip("/")
        if not raiz:
            raise ValueError("destino S3 sin bucket")
        return Destino("s3", raiz)
    return Destino("local", valor)


def clave(destino: Destino, dia: str, nombre: str) -> str:
    """Ruta relativa del objeto/archivo dentro del destino."""
    base = f"{dia}/{nombre}"
    if destino.tipo == "s3" and destino.prefijo:
        return f"{destino.prefijo}/{base}"
    return base


def _cliente_s3():
    """Import perezoso: solo se necesita en el camino de S3."""
    import boto3

    return boto3.client("s3")


def existe(destino: Destino, ruta: str) -> bool:
    if destino.tipo == "local":
        return (Path(destino.raiz) / ruta).exists()
    from botocore.exceptions import ClientError

    try:
        _cliente_s3().head_object(Bucket=destino.bucket, Key=ruta)
        return True
    except ClientError as exc:
        if exc.response["Error"]["Code"] in ("404", "NoSuchKey", "NotFound"):
            return False
        raise


def escribir(destino: Destino, ruta: str, crudo: bytes, comprimir: bool = True) -> int:
    """Escribe el contenido y devuelve los bytes escritos. Local es atómico."""
    datos = gzip.compress(crudo, compresslevel=6) if comprimir else crudo
    if destino.tipo == "local":
        final = Path(destino.raiz) / ruta
        final.parent.mkdir(parents=True, exist_ok=True)
        tmp = final.with_suffix(final.suffix + ".tmp")
        tmp.write_bytes(datos)
        tmp.replace(final)
        return final.stat().st_size
    _cliente_s3().put_object(
        Bucket=destino.bucket,
        Key=ruta,
        Body=datos,
        ContentType="application/json",
        **({"ContentEncoding": "gzip"} if comprimir else {}),
    )
    return len(datos)


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
    """Una línea legible para el log, para detectar snapshots vacíos."""
    if isinstance(payload, dict) and "symbols" in payload:
        simbolos = payload["symbols"]
        vivos = sum(1 for s in simbolos if s.get("status") in ("TRADING", None))
        return f"{len(simbolos)} símbolos ({vivos} TRADING)"
    if isinstance(payload, list):
        return f"{len(payload)} registros"
    return "formato inesperado"


def correr(destino: Destino, forzar: bool = False) -> dict:
    """Captura el snapshot del día. Devuelve un resumen de lo ocurrido."""
    ahora = datetime.now(UTC)
    dia = ahora.strftime("%Y-%m-%d")
    print(f"[{ahora.isoformat(timespec='seconds')}] snapshot {dia} -> {destino.tipo}:{destino.raiz}")

    escritos, omitidos, fallos = [], [], []
    for nombre, url in ENDPOINTS.items():
        ruta = clave(destino, dia, f"{nombre}.json.gz")
        try:
            if not forzar and existe(destino, ruta):
                print(f"  = {nombre}: ya existe, se omite")
                omitidos.append(nombre)
                continue
            crudo = fetch(url)
            payload = json.loads(crudo)  # valida que sea JSON antes de guardar
            tam = escribir(destino, ruta, crudo)
            print(f"  + {nombre}: {resumen(nombre, payload)}, {tam / 1024:.0f} KB -> {ruta}")
            escritos.append(nombre)
        except Exception as exc:  # noqa: BLE001 - se reporta y se sigue con el resto
            fallos.append(nombre)
            print(f"  ! {nombre}: FALLO - {exc}", file=sys.stderr)

    # Marca de tiempo exacta de la corrida, para auditar huecos.
    if not fallos:
        escribir(destino, clave(destino, dia, "_ok"), ahora.isoformat(timespec="seconds").encode(), comprimir=False)

    return {"dia": dia, "escritos": escritos, "omitidos": omitidos, "fallos": fallos}


def lambda_handler(event, context):
    """Punto de entrada en AWS Lambda.

    Lanza excepción si algún endpoint falló, para que la invocación se marque
    como error y el reintento de EventBridge se dispare.
    """
    destino = parse_destino(os.environ.get("SNAPSHOT_OUT", DEFAULT_OUT))
    forzar = os.environ.get("SNAPSHOT_FORCE", "").lower() in ("1", "true", "yes")
    if isinstance(event, dict):
        forzar = forzar or bool(event.get("force"))
    resultado = correr(destino, forzar=forzar)
    if resultado["fallos"]:
        raise RuntimeError(f"endpoints fallidos: {', '.join(resultado['fallos'])}")
    return resultado


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--out", default=os.environ.get("SNAPSHOT_OUT", DEFAULT_OUT),
                        help=f"ruta local o s3://bucket/prefijo (default: {DEFAULT_OUT})")
    parser.add_argument("--force", action="store_true", help="re-descargar aunque el snapshot de hoy ya exista")
    args = parser.parse_args()

    resultado = correr(parse_destino(args.out), forzar=args.force)
    if resultado["fallos"]:
        print(f"  {len(resultado['fallos'])} de {len(ENDPOINTS)} endpoints fallaron", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
