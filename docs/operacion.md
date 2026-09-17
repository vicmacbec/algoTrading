# Operación

## Entorno local

El entorno se gestiona con **uv** (no con pip ni conda: el Python del sistema no tiene `pip` ni
`ensurepip`). Python objetivo: **3.12**.

```bash
# Instalar uv una sola vez, sin sudo
curl -LsSf https://astral.sh/uv/install.sh | env UV_INSTALL_DIR="$HOME/.local/bin" sh

# Crear/sincronizar el entorno desde el lockfile
uv sync

# Correr cualquier cosa dentro del entorno
uv run python src/data/snapshot_universe.py
uv run pytest
uv run ruff check src test
```

`uv.lock` es la fuente de verdad de las versiones y **se versiona**. Para agregar una
dependencia: `uv add <paquete>`, que actualiza `pyproject.toml` y el lockfile a la vez.

## Credenciales

**Nunca dentro del repo ni de `~/Drive`**: Google Drive sincroniza esa carpeta a la nube y
conserva historial de versiones.

```bash
mkdir -p ~/.config/algotrading
cp .env.example ~/.config/algotrading/.env
chmod 600 ~/.config/algotrading/.env
# editar y poner los valores reales
```

Reglas de las llaves de Binance:

- Dos pares separados: uno de **solo lectura** para investigación y otro con permiso de **trade**
  para paper y producción.
- **Ninguna con permiso de retiro.**
- Con **IP allowlist** cuando se opere desde una IP fija.
- En producción (fase de AWS) pasan a Secrets Manager, no a variables de entorno del runtime.

## Snapshot diario del universo (crítico)

Captura el estado point-in-time de los mercados. **Es el único dato del proyecto que no se puede
reconstruir hacia atrás**: la API solo responde por los símbolos vivos hoy.

```bash
python3 src/data/snapshot_universe.py            # idempotente: si ya existe el de hoy, no repite
python3 src/data/snapshot_universe.py --force    # re-descarga
```

Ya está instalado en cron, **dos veces al día**:

```
5 9,21 * * * cd /home/vicmacbec/Drive/Codigos/AlgoTrading && /usr/bin/python3 \
    src/data/snapshot_universe.py >> data/raw/snapshots/cron.log 2>&1
```

Cron usa la **hora local** (`America/Mexico_City`), no UTC: dispara a las 09:05 y 21:05 CST,
es decir 15:05 y 03:05 UTC. Son dos corridas porque el script es idempotente —la segunda no
descarga nada si la primera funcionó— y así un día en que el equipo esté apagado a una de las
dos horas no se pierde. El nombre de la carpeta siempre se calcula en UTC.

Usa solo la biblioteca estándar a propósito, para que no dependa del entorno de uv.

**Verificar que no haya huecos** (un día sin `_ok` es un día perdido para siempre):

```bash
ls data/raw/snapshots | head -30
find data/raw/snapshots -maxdepth 1 -type d -mtime -30 '!' -exec test -e '{}/_ok' ';' -print
tail -20 data/raw/snapshots/cron.log
```

Si el equipo estuvo apagado varios días, no hay forma de recuperar esos snapshots: se documenta
el hueco y se sigue. Por eso conviene mover este job a la nube en cuanto haya infraestructura.

## Pruebas

```bash
uv run pytest -q          # toda la suite
uv run pytest test/data   # un módulo
```

Toda función nueva en `src/` necesita su prueba en `test/`, siguiendo el mismo árbol de
carpetas.

## Datos históricos

La descarga masiva no usa la API sino los dumps públicos de `data.binance.vision`, que no
requieren autenticación. Rutas verificadas:

```
data/spot/monthly/klines/<SYMBOL>/<INTERVAL>/<SYMBOL>-<INTERVAL>-<YYYY-MM>.zip
data/spot/daily/klines/<SYMBOL>/<INTERVAL>/<SYMBOL>-<INTERVAL>-<YYYY-MM-DD>.zip
data/futures/um/monthly/klines|fundingRate/...
data/futures/um/daily/metrics/...            (open interest)
```

El histórico llega hasta 2017-08 para BTCUSDT y ETHUSDT. Cada archivo trae su `.CHECKSUM`, que
debe verificarse al descargar. El módulo de ingesta se implementa en la Fase 1.

## Producción en AWS (pendiente, Fase 6-7)

El diseño aprobado es **Lambda arm64 + EventBridge Scheduler** cada 4 horas (dentro del free
tier), con S3 para datos y Secrets Manager para credenciales: del orden de 1.5 a 5 USD al mes.

Queda **descartado** el estimado de `AWS/My_AWS_Estimate.csv` (t4g.2xlarge + 1 TB de S3 =
55.59 USD/mes): cuesta más que el capital que se va a operar. Lo que permite prescindir de una
instancia encendida es que **el stop loss vive en el exchange como orden OCO**, no en el bot: si
el proceso no corre, la protección sigue puesta.

Las guías de EC2, RStudio Server y S3 siguen en `AWS/` (fuera de git) y aplican al esquema
anterior.

## Legacy en R

El código de `src/legacy_r/` está congelado y **hoy no corre**: no hay R instalado en el equipo.
Para reproducir algo puntual del pasado: `sudo apt install r-base-core` y los paquetes que
declare el script.

La instancia EC2 (`~/algoTrading/`) todavía tiene un crontab apuntando a rutas previas a la
migración y operando pares BUSD sin datos desde 2023. **Debe apagarse**; está registrado en
[`pendientes.md`](pendientes.md).
