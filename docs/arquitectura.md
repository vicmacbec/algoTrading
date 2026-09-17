# Arquitectura

## Componentes

```mermaid
flowchart TD
    A[Binance API<br/>paquete binancer] -->|binance_klines: velas OHLCV| B[Script de estrategia en R<br/>data.table + TTR]
    B --> C[Indicadores<br/>EMA, ATR, MA Slope, BBands, MACD, RSI]
    C --> D[Señales / flags de alerta]
    D --> E[Backtest vela a vela<br/>órdenes simuladas + stop loss]
    E --> F[(DataOut/&lt;Estrategia&gt;/<br/>CSV de órdenes y velas)]
    C --> G[Modelo XGBoost<br/>ml_tradingRules.R]
    G --> F
    F --> H[Gráficos<br/>ggplot2 / plotly]
    I[cron en EC2] -->|Rscript con el par como argumento| B
    F -.->|aws.s3| J[(Bucket S3<br/>algotrading-vicmacbec)]
    K[config.yml] -->|credenciales| B
```

- **Ingesta:** no hay base de datos ni capa de persistencia propia. Cada corrida pide las velas
  a Binance con `binance_klines()` y, en el script productivo, las concatena a los CSV
  históricos que ya existen en `DataOut/`.
- **Cómputo:** todo en memoria con `data.table`; los indicadores vienen de `TTR` salvo el MA
  Slope, que se calcula a mano (ver [`reglas-negocio.md`](reglas-negocio.md)).
- **Persistencia:** archivos CSV en `DataOut/`, una carpeta por estrategia. El estado del
  backtest (`allOrders`, `allData`) vive en esos CSV: el script productivo los lee, los
  reescribe completos y no guarda nada más.
- **Ejecución programada:** un wrapper `.sh` llamado desde crontab en la EC2 invoca el `.R` con
  el símbolo como argumento y redirige la salida a un log por corrida.
- **Distribución:** `aws.s3` para subir y leer los CSV desde el bucket, de modo que la EC2 y la
  máquina local compartan los mismos datos.

## Decisiones tomadas

**CSV en vez de base de datos.** El volumen es de miles de velas por par y el consumo es
siempre "leer todo, recalcular, reescribir". Un CSV por estrategia mantiene el proyecto
portable entre la laptop y la EC2 sin infraestructura extra; el costo es que no hay escrituras
concurrentes ni consultas parciales.

**Dos versiones del mismo algoritmo.** `MASlope_ATRStopL.R` (investigación, todos los pares,
gráficos) y `MASlope_ATRStopL_Prod.R` (un par, sin gráficos, ~8 s). La versión productiva se
mantiene mínima para que quepa en una corrida de cron; la de investigación carga librerías
pesadas (plotly, patchwork) que en producción no hacen falta.

**Rutas absolutas conmutadas a mano.** Los scripts declaran las rutas de local
(`~/Drive/Codigos/AlgoTrading/`) y de la EC2 (`~/algoTrading/`), y se cambia de entorno
comentando líneas. Es la fuente de error más probable al desplegar; está registrado como
pendiente en [`pendientes.md`](pendientes.md).

**Órdenes reales desactivadas.** Las llamadas a `binance_new_order()` están comentadas en el
script productivo: hoy el sistema es paper trading y su única salida son los CSV. Las funciones
que sí construyen órdenes válidas (cantidades, `minNotional`, decimales) viven en
[`src/Tests/binance.R`](../src/Tests/binance.R) y son el punto de partida para operar en real.

**Migración de `Scripts/` a `src/` (2026-09-16).** Se adoptó la convención de la skill
`project-structure`. `git mv` conservó el historial; las rutas del wrapper `.sh`, del script
productivo, del `.gitignore` y del README se actualizaron en el mismo commit. El despliegue de
la EC2 y su crontab siguen apuntando a la ruta vieja y deben actualizarse por separado.

**R congelado, sistema nuevo en Python (2026-09-17).** El proyecto pasa a un sistema
cuantitativo en Python y el código R queda archivado en `src/legacy_r/` sin mantenimiento. Las
razones son medidas, no de preferencia: el edge de la estrategia actual (+0.384% bruto por
operación) es del mismo orden que su costo (0.15%–0.40% por round trip, 4.5 operaciones por par
al mes), el modelo de ML tiene fugas que invalidan sus métricas, y el universo BUSD está muerto.
El plan completo, con fases y criterios de paso, vive en el plan de trabajo aprobado; esta
entrada solo registra la decisión arquitectónica.

Consecuencias inmediatas:

- **Ingesta propia point-in-time.** Se captura a diario `exchangeInfo` y `ticker/24hr` de spot y
  de perpetuos USDⓈ-M (`src/data/snapshot_universe.py`, por cron local). La API solo responde por
  los símbolos vivos hoy, así que sin este snapshot el sesgo de supervivencia es irreparable
  hacia atrás. Es deliberadamente de biblioteca estándar: debe correr aunque el entorno falle.
- **El histórico masivo no se baja por API** sino de los dumps públicos de `data.binance.vision`
  (sin autenticación), incluidos `fundingRate` y `metrics` de futuros como features de régimen.
- **Almacenamiento en Parquet consultado con DuckDB**, en vez de CSV reescritos completos. El
  estado deja de ser un archivo que se sobrescribe y pasa a ser inmutable por partición.
- **El entorno se fija con `uv` y lockfile**, con Python 3.12 y las dependencias declaradas en
  `pyproject.toml`.
- **Las credenciales salen del repo y de Google Drive** a `~/.config/algotrading/.env`. Ver
  `docs/operacion.md`.
