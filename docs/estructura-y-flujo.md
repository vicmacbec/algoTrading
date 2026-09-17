# Estructura y flujo

Proyecto en **R** (se abre con [`AlgoTrading.Rproj`](../AlgoTrading.Rproj)) para investigar
estrategias de trading de criptomonedas con datos de **Binance** (paquete `binancer`).

Los scripts no forman un paquete: son análisis que se corren a mano en RStudio, sección por
sección. La excepción es `MASlope_ATRStopL_Prod.R`, que se ejecuta con `Rscript` desde cron en
una instancia **AWS EC2** (ver [`operacion.md`](operacion.md)).

## Flujo general

```
Binance API (binance_klines)
        │  velas OHLCV (1d / 4h / 5m)
        ▼
Indicadores técnicos (TTR, data.table): EMA, ATR, MA Slope, BBands, MACD, RSI, SSL...
        ▼
Reglas / señales (flags de alerta)  ──►  Backtest vela a vela (órdenes simuladas, stop loss, fee)
        │                                      ▼
        │                              Resumen de rendimiento por par (yield, cumYield, riskReward)
        ▼                                      ▼
Modelo ML (XGBoost) sobre indicadores     DataOut/<Estrategia>/  (CSV + imágenes)
                                               ▼
                                   (Producción) cron en EC2 → S3
```

Las reglas de cada estrategia y sus fórmulas están en [`reglas-negocio.md`](reglas-negocio.md);
los componentes y las decisiones de diseño, en [`arquitectura.md`](arquitectura.md).

Todos los scripts siguen la plantilla [`src/template.R`](../src/template.R), con estas secciones:

```
Load libraries → Functions → Load data → Initial parameters →
Strategy → Alert flags → Testing strategy → Plots → Saving data → Tests
```

## Carpetas

| Carpeta | Qué contiene |
|---|---|
| `src/` | Todo el código R y los wrappers `.sh`. |
| `src/Strategies/` | Una estrategia por archivo, con su backtest. |
| `src/Tests/` | Pruebas exploratorias de la API, indicadores, gráficos, comisiones y S3. |
| `docs/` | Esta documentación (6 archivos fijos, ver skill `docs-maintainer`). |
| `DataIn/` | Datos de entrada. Hoy solo `.gitkeep` y el acceso a `tradingJournal` (Google Sheet): los datos de mercado se bajan en vivo de la API. |
| `DataOut/` | Resultados por estrategia (CSV, logs e imágenes). Casi todo ignorado por git. |
| `AWS/` | Guías para montar EC2, RStudio Server, S3, Lambda y crontab. Ignorada por git. |
| `Credentials/` | Llaves de Binance, token de GitHub y credenciales AWS. Ignorada por git. |

No existe `test/`: el proyecto todavía no tiene pruebas unitarias (ver
[`pendientes.md`](pendientes.md)).

## Archivos más importantes

### `src/`

| Archivo | Uso | Uso estimado |
|---|---|---|
| [`template.R`](../src/template.R) | Plantilla base para una estrategia nueva. | Solo al crear un script. |
| [`binance.R`](../src/binance.R) | Primer prototipo: baja velas y calcula medias móviles, Bollinger, MACD y RSI con sus flags. | Histórico, ya cubierto por las estrategias. |
| `keys.R` *(ignorado por git)* | Pruebas de manejo de credenciales con `keyring` y `config`. | Puntual. |

### `src/Strategies/`

La mayoría salen del ranking de *Trading Zone*; cada script lleva el enlace al video.

| Archivo | Qué hace | Uso estimado |
|---|---|---|
| [`BB_RSI_MACD.R`](../src/Strategies/BB_RSI_MACD.R) | Todos los pares BUSD en 1d. RSI como señal principal, confirmada con Bollinger, y MACD para el cambio de tendencia. | Manual, exploratorio. |
| [`pumNGo.R`](../src/Strategies/pumNGo.R) | Estrategia "pump and go" sobre todos los pares en 1d. | Manual, exploratorio. |
| [`SSL.R`](../src/Strategies/SSL.R) / [`SSL_EMA.R`](../src/Strategies/SSL_EMA.R) | Canal SSL (con filtro EMA en la segunda versión) en velas de 5m; backtest por par. `SSL_EMA` guarda `DataOut/SSL_EMA/summaryAll.csv`. | Manual; ~15 min por corrida completa. |
| [`RSI_MA_MA.R`](../src/Strategies/RSI_MA_MA.R) | Solo la plantilla con el enlace al video: sin implementar. | Sin uso. |
| [`MASlope_ATRStopL.R`](../src/Strategies/MASlope_ATRStopL.R) | Versión de investigación de la estrategia principal: backtest de todos los pares en 4h; genera los CSV históricos y los gráficos (plotly). | Manual, al reajustar parámetros. |
| [`MASlope_ATRStopL_Prod.R`](../src/Strategies/MASlope_ATRStopL_Prod.R) | **Único script productivo.** Recibe el par por argumento, lee `config.yml`, agrega las últimas velas de 4h a los CSV históricos y vuelve a correr la lógica (~8 s). Las llamadas a `binance_new_order` están comentadas: hoy solo simula. | Por cron cada 4 h cuando la EC2 está encendida. |
| [`MASlope_ATRStopL_Prod.sh`](../src/Strategies/MASlope_ATRStopL_Prod.sh) | Wrapper de cron: ejecuta el `.R` con un símbolo (`FXSBUSD`) y manda la salida a `DataOut/MASlope_ATRStopLoss/allLogs/`. | Igual que el anterior. |
| [`ml_tradingRules.R`](../src/Strategies/ml_tradingRules.R) | **En curso** (issue #9). ~10 años de velas diarias, indicadores como features, XGBoost con métrica de especificidad, cross-validation, grid search, matriz de confusión, ROC/AUC e índice de Youden. | En desarrollo activo. |

### `src/Tests/`

| Archivo | Uso |
|---|---|
| [`binance.R`](../src/Tests/binance.R) | El más importante de la carpeta: órdenes `LIMIT` y `STOP_LOSS_LIMIT` en modo test, cálculo de cantidades válidas según los filtros del par (`minNotional`, decimales) y las funciones `buyTrade()` y de venta. Base para operar en real. |
| [`Indicators.R`](../src/Tests/Indicators.R) | Pruebas de indicadores y flags (MA Slope, ATR stop loss). |
| [`plots.R`](../src/Tests/plots.R) | Gráficos de velas con plotly y tidyquant. |
| [`fees.R`](../src/Tests/fees.R) | Cálculo de comisiones de compra y venta. |
| [`awsS3.R`](../src/Tests/awsS3.R) | Conexión con `aws.s3`: sube y lee archivos del bucket `algotrading-vicmacbec`. |
| [`crontabTime.sh`](../src/Tests/crontabTime.sh) | Prueba mínima para verificar que cron corre. |

### Raíz

| Archivo | Uso |
|---|---|
| [`README.md`](../README.md) | Descripción breve del repo; la lista de estrategias está incompleta. |
| [`binancePairsBUSD.R`](../binancePairsBUSD.R) | No es un script: es la salida pegada con la lista de ~300 pares BUSD de 2022. |
| `config.yml` *(ignorado por git)* | Credenciales de Binance y AWS que leen los scripts. |

## Salidas en `DataOut/`

- `MASlope_ATRStopLoss/` — `Orders/allOrders_year_20220421.csv` (órdenes simuladas con su
  rendimiento), `Trades/allData_year_4h_20220421.csv` (velas con indicadores), `myTrades/`,
  `Logs/` y `allLogs/` (salida del cron, hoy vacías) e `Images/`.
- `MLRules/` — `GridSearch/` con los resultados de la búsqueda de hiperparámetros e `Images/`
  (importancia de features, bias-variance, ROC). Solo `1featureImportance.png` está versionado.
- `SSL_EMA/` — `summaryAll.csv` con el comparativo por símbolo.
- `tradingJournal.xlsx` y `Check.xlsx` — bitácora manual.

Los porcentajes de uso de arriba son estimaciones a partir del diseño de cada script y de la
configuración del cron: las carpetas de logs están vacías, así que no hay ejecuciones recientes
registradas de las que derivarlos.
