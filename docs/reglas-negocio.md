# Reglas de negocio

Dominio: trading algorítmico de criptomonedas en Binance. El objetivo declarado es **ganar
dinero**, no producir un estudio: más de **1 % mensual neto** de comisiones y gastos.

## El presupuesto de costos manda sobre todo lo demás

Round-trip realista en spot USDT, por segmento de liquidez:

| Segmento | Fees (pagando con BNB) | Spread | Slippage | **Total ida y vuelta** |
|---|---|---|---|---|
| Top 20 por volumen | 0.150 % | 0.01–0.03 % | <0.02 % | **~0.18–0.20 %** |
| Puestos 20–80 | 0.150 % | 0.05–0.12 % | 0.02–0.05 % | **~0.25–0.35 %** |
| Cola | 0.150 % | 0.15–0.40 % | 0.05–0.20 % | **~0.40–0.75 %** |

De ahí sale la regla que decide qué estrategias son siquiera admisibles: si se acepta gastar como
mucho la mitad del objetivo (0.5 % mensual) en fricción, el sistema se permite **~2.5 round-trips
al mes por unidad de capital** en pares líquidos. Con cinco posiciones concurrentes eso es un
**holding medio de ~8 días**.

**La cadencia de decisión es de 4 horas; la de rotación es de días.** Toda estrategia cuyo
backtest promedie menos de 4 días de holding queda descalificada por presupuesto antes de mirar
su Sharpe.

## Universo y ejecución

- **Universo de selección ≠ instrumento de ejecución.** El universo se filtra por liquidez,
  volumen, spread, antigüedad e historia disponible; la ejecución se hace contra USDT.
- **Quote asset: USDT** (493 pares spot). Se descartó operar contra BNB: solo existen 7 pares, y
  el descuento del 25 % en comisiones se obtiene **pagando las fees con BNB**, sin necesidad de
  pares `*BNB`.
- **Universo point-in-time obligatorio.** Los pares delistados permanecen en el backtest con su
  desenlace real. Excluirlos *es* el sesgo de supervivencia.
- **Spot solo-long primero**, perpetuos USDⓈ-M después (ahí entran cortos y, mucho más tarde,
  apalancamiento).
- `minNotional` de 5 USDT: una orden por debajo no se toma y se registra como rechazada por
  capital. Contar esos rechazos es como se descubre el capital mínimo viable.

## Etiquetado: triple barrera

Un evento se etiqueta por lo que ocurre primero entre tres barreras:

- **Superior (TP):** `+k_u · σ_t`
- **Inferior (SL):** `−k_d · σ_t`
- **Temporal:** `H` velas de 4 h, con `H ∈ {24, 42, 90}` (4, 7 y 15 días)

`σ_t` es la volatilidad realizada EWM de los retornos de 4 h, de modo que las barreras son
comparables entre pares y entre regímenes.

**La resolución intrabarra con velas de 1 m no es opcional:** con velas de 4 h no se sabe si se
tocó antes el máximo o el mínimo. Si ambas barreras caen en el mismo minuto se asume **SL**
(criterio pesimista) y se reporta la fracción de eventos ambiguos; si supera el 5 %, las barreras
son demasiado estrechas para esta cadencia.

Cada evento produce además **MFE** (máxima excursión favorable) y **MAE** (máxima adversa), que
son las que permiten calibrar dónde poner el stop en vez de adivinar un `1.5 · ATR`.

## Indicadores disponibles como features

| Indicador | Cálculo | Por qué sirve |
|---|---|---|
| `ohlc4` | `(open + high + low + close) / 4` | Precio típico de la vela. |
| `ema` | `EMA(ohlc4, 55)` | Tendencia. |
| `atr` | `ATR(high, low, close, 14)` | Volatilidad; base del stop. |
| `maSlope` | `(180/π) · atan((ema − ema_lag) / atr)` | Pendiente de la EMA **en grados**. Ya viene normalizada por volatilidad, así que es comparable entre pares: por eso sobrevive del sistema anterior. |
| Bollinger | `BBands(close)` → `dn`, `up`, `pctB` | Posición dentro del canal. |
| MACD | `MACD(close)` → `macd`, `signal` | Momentum. |
| RSI | `RSI(close, n = 6)` | Se usa 6, no el 14 por defecto, para igualar los gráficos de Binance. |
| **Flujo de órdenes** | `taker_buy_quote / quote_volume`, su z-score, CVD proxy, ticket medio | Viene gratis en las propias velas de Binance y casi nadie lo explota: mide el desbalance entre compradores y vendedores agresivos. |

**Regla dura de construcción:** nunca niveles de precio crudos como feature. Todo va normalizado
por volatilidad o convertido a rango percentil dentro del universo en cada `t`. Los niveles no
son estacionarios y el modelo acaba memorizando el régimen de precios.

## Métricas y umbrales de paso

Ninguna estrategia avanza de fase sin cumplirlos **todos**:

| Métrica | Umbral |
|---|---|
| Sharpe neto anualizado, walk-forward fuera de muestra | ≥ 1.0 |
| Sharpe con costos al doble | ≥ 0.5 y retorno > 0 |
| Deflated Sharpe Ratio | > 0 (o PBO < 0.3) |
| Retorno mensual mediano neto | ≥ 1.0 % |
| Meses positivos | ≥ 8 de cada 12 |
| Máximo drawdown | ≤ 25 % |
| Operaciones fuera de muestra | ≥ 150 |
| Correlación con BTC | < 0.6 |
| Alpha vs BTC | t-stat > 2 |

**El listón no es cero, es el buy & hold de BTC ajustado por riesgo.** Un sistema que rinde menos
que mantener BTC, con más trabajo y más riesgo operativo, no descubrió alpha: descubrió una forma
cara de tener beta.

---

## Legado: el sistema en R (congelado)

Las reglas de abajo describen el sistema anterior, que ya no se ejecuta. Se conservan porque la
estrategia se porta como *baseline* de referencia y porque explican de dónde salen los datos de
`DataOut/`.

**MA Slope + ATR Stop Loss**, solo largo, en velas de 4 h:

- **Entrada:** `maSlope >= 3` grados sin un cruce ya activo; se abre al cierre de esa vela.
- **Stop loss:** `close − 1.5 · atr` en el momento de la entrada, estático.
- **Cierre:** el `low` perfora el stop, o la pendiente deja de ser alcista **y** el cierre está
  por encima de la entrada.

```
rate            = (price_f − price_0) / price_0
realRate        = rate · (1 − fee) − 2 · fee        # fee = 0.00075
yield           = 1 + realRate
cumYield        = cumprod(yield) por símbolo
```

**Tres defectos medidos que invalidan sus resultados**, y que el sistema nuevo corrige por
diseño:

1. `cumYield` acumula por símbolo como si cada par dispusiera del 100 % del capital. No es un
   retorno de portafolio: con diez pares activos implica diez veces el capital.
2. El backtest ejecuta al cierre de la misma vela que genera la señal —en la realidad se decide
   *después* de ver ese cierre— y llena los stops exactamente en el nivel, sin hueco.
3. El edge medido sobre sus 11,488 órdenes es de +0.384 % bruto por operación con 4.5
   operaciones por par al mes: del mismo orden que el costo, y negativo a costos de altcoin.

**El modelo de ML (`ml_tradingRules.R`)** etiquetaba `1` si el máximo de los 7 días siguientes
superaba en 5 % la apertura. Tenía tres fugas apiladas: split aleatorio sobre serie temporal,
ventana de etiqueta que incluía la vela actual (parcialmente observable al decidir) y features de
nivel de precio crudo. Sus métricas están infladas y no deben citarse.

**BUSD.** Todo el universo del sistema anterior eran pares `*BUSD`, que Binance descontinuó entre
2023 y 2024. Por eso `DataOut/` sirve solo como fixture de regresión, no como fuente de datos.
