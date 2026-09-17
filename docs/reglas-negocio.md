# Reglas de negocio

Dominio: trading algorítmico de criptomonedas en Binance. Todos los pares son contra **BUSD**
(ver la nota al final).

## Términos

| Término | Definición |
|---|---|
| **Vela / kline** | Registro OHLCV de un intervalo (`1d`, `4h`, `5m`): apertura, máximo, mínimo, cierre, volumen y número de trades. |
| **Par** | Símbolo negociado, p. ej. `FXSBUSD`. |
| **Orden** | Operación simulada del backtest, con precio de entrada (`price_0`), de salida (`price_f`), stop loss y resultado. |
| **Orden activa** | `active == 1`: abierta, todavía sin cerrar por stop loss ni por toma de ganancias. |
| **Long** | Única dirección implementada: se compra esperando que suba. Las variables `shortStopLoss` se calculan pero no se operan. |

## Indicadores

| Indicador | Cálculo | Dónde |
|---|---|---|
| `ohlc4` | `(open + high + low + close) / 4` | Precio típico de la vela. |
| `ema` | `EMA(ohlc4, 55)` | Tendencia. |
| `atr` | `ATR(high, low, close, 14)` | Volatilidad; base del stop loss. |
| `maSlope` | `(180/π) · atan((ema − ema_lag) / atr)` | Pendiente de la EMA **en grados**, normalizada por volatilidad: mide la inclinación de la tendencia de forma comparable entre pares. |
| Bollinger | `BBands(close)` → `dn`, `up`, `pctB` | La media (`mavg`) se descarta por ser igual a `rollmean20`. |
| MACD | `MACD(close)` → `macd`, `signal` | `macdChange`: `"down"` si `macd < signal`. |
| RSI | `RSI(close, n = 6)` | Se usa 6, no el 14 por defecto, para igualar los gráficos de Binance. |
| Medias móviles | `frollmean` adaptativo de 5, 10, 20 y 51 periodos | La ventana adaptativa (`an()`) permite calcular las primeras velas sin `NA`. |

## Estrategia principal: MA Slope + ATR Stop Loss

Es la única que llegó a productivo. Opera solo en largo, en velas de 4 h.

**Entrada.** Cuando `maSlope >= 3` grados y no había un cruce activo, se abre una orden al
cierre de esa vela. Se marca el cruce como activo (`maSlopeCross = TRUE`) para no volver a
abrir en cada vela de la misma tendencia.

**Stop loss.** `longStopLoss = close − 1.5 · atr` en el momento de la entrada. Es estático: no
se sube conforme avanza el precio. El multiplicador `1.5` es el parámetro `multiplier`.

**Cierre.** Una orden activa se cierra cuando:
1. El `low` de una vela cae por debajo del stop loss → cierra en `stopLoss`, con
   `riskRewardRatio = 0`.
2. La pendiente deja de ser alcista (`maSlope < 3`) **y** el cierre está por encima del precio
   de entrada → cierra con ganancia al `close` de esa vela.

Si la pendiente se vuelve negativa y el precio está por debajo de la entrada, la orden se queda
abierta esperando el stop loss o una recuperación.

## Fórmulas de rendimiento

```
rate             = (price_f − price_0) / price_0
realRate         = rate · (1 − fee) − 2 · fee        # fee = 0.00075 (0.075 %)
yield            = 1 + realRate
cumYield         = cumprod(yield) por símbolo
riskRewardRatio  = ((close − price_0) / price_0) / ((price_0 − stopLoss) / price_0)
```

`fee` descuenta dos veces la comisión (compra y venta) más la parte proporcional sobre la
ganancia. `cumYield` es multiplicativo: el rendimiento acumulado de reinvertir todo el capital
en cada orden del mismo par.

`riskRewardRatio` solo se calcula en los cierres con ganancia; en un cierre por stop loss vale
0 por definición.

## Otras estrategias (backtest, no productivas)

- **BB_RSI_MACD.** El RSI es la señal principal (`RSI <= 30` sobreventa, `>= 70` sobrecompra).
  Si además el precio toca la banda de Bollinger (`low <= dn` o `high >= up`), la señal se
  confirma. Mientras el RSI siga en zona extrema se sigue promediando, y el cambio de tendencia
  se confirma con el cruce del MACD.
- **SSL y SSL_EMA.** Canal SSL en velas de 5 m; la segunda versión agrega un filtro de EMA para
  operar solo a favor de la tendencia.
- **pumNGo.** Detección de subidas bruscas en velas diarias.

## Regla del modelo de ML (`ml_tradingRules.R`)

**Target:** una vela diaria se etiqueta como `1` si el máximo de los **7 días siguientes**
supera en más de **5 %** el precio de apertura:

```
high7Days = frollapply(high, n = 7, FUN = max, align = "left")
increment = (high7Days − open) / open
target    = increment > 0.05
```

El umbral de 5 % se eligió mirando los percentiles de `increment`: alrededor de la mitad de las
semanas superan ese incremento, lo que deja las clases razonablemente balanceadas.

**Métrica:** se optimiza **especificidad**, no exactitud. Importa más evitar falsos positivos
(entrar en una operación que no sube) que capturar todas las subidas. El punto de corte de la
probabilidad se elige con el índice de Youden sobre la curva ROC.

## Nota sobre BUSD

Binance descontinuó BUSD entre 2023 y 2024. Los pares `*BUSD` que usan todos los scripts ya no
reciben datos nuevos, así que cualquier corrida actual devuelve series vacías o congeladas.
Migrar a USDT o USDC está registrado en [`pendientes.md`](pendientes.md).
