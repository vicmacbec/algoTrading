# Estructura y flujo

El proyecto está en transición: el sistema nuevo se construye en **Python** y el código **R
queda congelado** en `src/legacy_r/` (ver la decisión en [`arquitectura.md`](arquitectura.md)).

## Flujo objetivo

```
data.binance.vision (dumps públicos, sin auth)     API de Binance (diaria)
        │  klines 4h/1d/1m, fundingRate, metrics          │  exchangeInfo, ticker/24hr
        ▼                                                 ▼
   data/raw/  (Parquet particionado)              data/raw/snapshots/  (universo point-in-time)
        └───────────────────────┬─────────────────────────┘
                                ▼
                    src/features/  →  panel de features (multi-timeframe, cross-seccional)
                                ▼
                    src/labeling/  →  triple barrera resuelta con velas de 1m + MFE/MAE
                                ▼
                    src/strategies/ + src/validation/  →  reglas base, meta-modelo, purged CV
                                ▼
                    src/backtest/  →  backtest de portafolio con costos y capital finito
                                ▼
                    tracker SQLite + Streamlit  →  experimentos, backtests, operaciones
                                ▼
                    src/live/  →  paper trading y, después, ejecución real
```

La cadencia de decisión es de **4 horas**, pero la rotación esperada es de días: el presupuesto
de costos no admite operar cada vela (ver [`reglas-negocio.md`](reglas-negocio.md)).

## Carpetas

| Carpeta | Qué contiene | Estado |
|---|---|---|
| `src/data/` | Ingesta: snapshots del universo, descarga de dumps, normalización a Parquet, calidad de datos | En construcción |
| `src/features/` | Familias de features: precio, volatilidad, flujo de órdenes, cross-seccionales, régimen, multi-timeframe | Pendiente (Fase 4) |
| `src/labeling/` | Triple barrera, MFE/MAE, unicidad y pesos de muestra | Pendiente (Fase 4) |
| `src/validation/` | Purged K-fold con embargo, walk-forward, Deflated Sharpe, PBO, Monte Carlo | Pendiente (Fase 5) |
| `src/strategies/` | Reglas base (baselines) y meta-modelo | Pendiente (Fase 2) |
| `src/backtest/` | Motor de portafolio, ejecución, sizing, restricciones y métricas | Pendiente (Fase 3) |
| `src/live/` | Runner, gestión de órdenes, reconciliación, kill switch, reloj | Pendiente (Fase 6) |
| `src/services/` | Lógica reutilizable: exchange, almacenamiento, tracker, notificaciones, fiscal | Pendiente |
| `src/agentes/`, `src/tools/`, `src/prompts/`, `src/catalogos/`, `src/queries/` | Capa agéntica de **solo lectura**: reportes, triage y consulta del tracker | Pendiente |
| `src/legacy_r/` | Todo el código R anterior, sin mantenimiento | **Congelado** |
| `test/` | Pruebas unitarias, espejo de `src/` | En construcción |
| `configs/` | YAML versionado de estrategias, universos y costos | Vacío |
| `data/` | Artefactos de runtime: `raw/`, `curated/`, `features/`, `labels/`. **No versionado** | Activo |
| `docs/` | Esta documentación (6 archivos fijos) | Activo |
| `DataOut/MASlope_ATRStopLoss/` | Se conserva como **fixture de regresión** del motor nuevo | Congelado |
| `AWS/`, `Credentials/` | Guías de infraestructura y credenciales. Ignoradas por git | A migrar |

## Archivos más importantes

### Sistema nuevo

| Archivo | Uso | Frecuencia |
|---|---|---|
| [`src/data/snapshot_universe.py`](../src/data/snapshot_universe.py) | Captura diaria de `exchangeInfo` y `ticker/24hr` de spot y perpetuos. **Solo biblioteca estándar**, a propósito: debe correr aunque el entorno falle. Es el único dato irrecuperable hacia atrás. | Diaria por cron (00:05 UTC) |
| [`pyproject.toml`](../pyproject.toml) | Dependencias fijadas; `uv.lock` es la fuente de verdad exacta | Al cambiar dependencias |
| [`.env.example`](../.env.example) | Plantilla de credenciales; el archivo real vive fuera del repo | Referencia |

### Legacy en R (`src/legacy_r/`, congelado)

| Archivo | Qué hacía | Por qué se conserva |
|---|---|---|
| `Tests/binance.R` | Órdenes `LIMIT` y `STOP_LOSS_LIMIT`, cantidades válidas según `minNotional`, `stepSize` y decimales, `buyTrade()` | **El activo más valioso del repo**: esa lógica se porta a `src/services/exchange.py`, no se redescubre |
| `Strategies/MASlope_ATRStopL.R` | Backtest de la estrategia MA Slope en 4h | Regla a portar como baseline de referencia |
| `Strategies/MASlope_ATRStopL_Prod.R` + `.sh` | Versión productiva por cron en EC2 | Referencia del diseño operativo; el cron debe apagarse |
| `Strategies/ml_tradingRules.R` | Modelo XGBoost | Referencia de **qué no repetir**: split aleatorio, etiqueta observable y features de nivel de precio |
| `Strategies/{BB_RSI_MACD,SSL,SSL_EMA,pumNGo}.R` | Estrategias exploratorias | Ideas para las baselines |

Ningún script de R está en uso: hoy ni siquiera hay R instalado en el equipo.

## Salidas

- `data/raw/snapshots/YYYY-MM-DD/` — cuatro JSON comprimidos por día (~740 KB), más `_ok` con la
  marca de tiempo de la corrida. La ausencia de `_ok` delata un día incompleto.
- `data/raw/snapshots/cron.log` — bitácora del cron.
- `DataOut/MASlope_ATRStopLoss/Orders/allOrders_year_20220421.csv` — 11,488 órdenes simuladas que
  sirven de fixture de regresión para validar el motor de backtest nuevo.
