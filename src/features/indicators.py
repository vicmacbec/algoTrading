"""Indicadores de tendencia y de volumen, con las definiciones de TTR.

Se incluyen solo los que aportan información distinta de la que ya dan los
estimadores de volatilidad y el flujo de órdenes:

- **ATR** y **ADX/DI**: fuerza de tendencia, que no es lo mismo que dirección.
- **MFI, CMF, OBV**: volumen firmado por heurística. Quedan como referencia; el
  flujo agresivo de `taker_buy` es mejor medida porque viene del exchange y no
  de adivinar el signo por el precio.

**Lo que deliberadamente no está:** `ZigZag`. Reubica sus pivotes con precios
posteriores, así que mira al futuro por construcción. Un feature así no da
error: da un backtest espectacular que muere en vivo.

**Desviación documentada de TTR.** En un tramo sin ningún movimiento
direccional, TTR calcula `0/0` en los DI y el DX, y ese NaN se propaga por la
recursión de la EMA hasta el final de la serie. Aquí esos casos valen 0 ("no
hay tendencia"), que es su significado económico, y la serie sigue siendo útil.
Con precios normales el resultado es idéntico al de TTR.
"""

from __future__ import annotations

import numpy as np

from src.features.rolling import como_array, ema, lag, roc, run_sum, wilder_sum


def _ohlcv(*series) -> tuple[np.ndarray, ...]:
    arrays = tuple(como_array(s) for s in series)
    if len({len(a) for a in arrays}) != 1:
        raise ValueError("todas las series deben tener la misma longitud")
    return arrays


def true_range(high, low, close) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Rango verdadero: incluye el hueco respecto al cierre anterior.

    La primera vela no tiene cierre previo, así que vale NaN, igual que en TTR.
    Eso retrasa en una posición el primer valor de todo lo que se construye
    encima (ATR, ADX).
    """
    h, l, c = _ohlcv(high, low, close)
    c1 = lag(c, 1)
    alto = np.maximum(h, c1)   # propaga NaN, como pmax(na.rm = FALSE)
    bajo = np.minimum(l, c1)
    return alto - bajo, alto, bajo


def atr(high, low, close, n: int = 14) -> np.ndarray:
    """Average True Range: EMA de Wilder sobre el rango verdadero."""
    tr, _, _ = true_range(high, low, close)
    return ema(tr, n, wilder=True)


def adx(high, low, close, n: int = 14) -> dict[str, np.ndarray]:
    """Índice direccional medio de Wilder, con sus componentes.

    Devuelve `di_pos` y `di_neg` (dirección, de 0 a 100), `dx` y `adx` (fuerza
    de la tendencia, sin signo). El primer ADX aparece en la posición `2n-1`:
    `n` velas para el DX y otras `n` para suavizarlo.
    """
    h, l, c = _ohlcv(high, low, close)
    dh = h - lag(h, 1)
    dl = lag(l, 1) - l
    nulo = (dh == dl) | ((dh < 0) & (dl < 0))
    dmi_pos = np.where(nulo, 0.0, np.where(dh > dl, dh, 0.0))
    dmi_neg = np.where(nulo, 0.0, np.where(dh < dl, dl, 0.0))
    sin_previo = np.isnan(dh) | np.isnan(dl)
    dmi_pos[sin_previo] = np.nan
    dmi_neg[sin_previo] = np.nan

    tr, _, _ = true_range(h, l, c)
    tr_suma = wilder_sum(tr, n)
    with np.errstate(divide="ignore", invalid="ignore"):
        di_pos = 100.0 * wilder_sum(dmi_pos, n) / tr_suma
        di_neg = 100.0 * wilder_sum(dmi_neg, n) / tr_suma
    # Rango verdadero nulo en toda la ventana: no hay movimiento, no hay dirección.
    plano = (tr_suma == 0)
    di_pos[plano] = 0.0
    di_neg[plano] = 0.0

    with np.errstate(divide="ignore", invalid="ignore"):
        dx = 100.0 * np.abs(di_pos - di_neg) / (di_pos + di_neg)
    dx[(di_pos + di_neg) == 0] = 0.0

    return {"di_pos": di_pos, "di_neg": di_neg, "dx": dx, "adx": ema(dx, n, wilder=True)}


def clv(high, low, close) -> np.ndarray:
    """Close Location Value: dónde cerró la vela dentro de su rango, en [-1, 1].

    +1 es cierre en el máximo, -1 en el mínimo. Una vela sin rango vale 0.
    """
    h, l, c = _ohlcv(high, low, close)
    with np.errstate(divide="ignore", invalid="ignore"):
        valor = ((c - l) - (h - c)) / (h - l)
    valor[~np.isfinite(valor)] = 0.0
    return valor


def cmf(high, low, close, volume, n: int = 20) -> np.ndarray:
    """Chaikin Money Flow: volumen ponderado por la posición del cierre."""
    h, l, c, v = _ohlcv(high, low, close, volume)
    with np.errstate(divide="ignore", invalid="ignore"):
        return run_sum(clv(h, l, c) * v, n) / run_sum(v, n)


def mfi(high, low, close, volume, n: int = 14) -> np.ndarray:
    """Money Flow Index: un RSI ponderado por volumen, de 0 a 100.

    Sigue los casos límite de TTR: solo flujo positivo da 100, y ningún flujo
    en ninguna dirección da 50.
    """
    h, l, c, v = _ohlcv(high, low, close, volume)
    tipico = (h + l + c) / 3.0
    previo = lag(tipico, 1)
    flujo = tipico * v
    positivo = np.where(tipico > previo, flujo, 0.0)
    negativo = np.where(tipico < previo, flujo, 0.0)
    positivo[np.isnan(previo)] = np.nan
    negativo[np.isnan(previo)] = np.nan

    num = run_sum(positivo, n)
    den = run_sum(negativo, n)
    with np.errstate(divide="ignore", invalid="ignore"):
        valor = 100.0 - 100.0 / (1.0 + num / den)
    valor[den == 0] = 100.0
    valor[(den == 0) & (num == 0)] = 50.0
    return valor


def obv(close, volume) -> np.ndarray:
    """On Balance Volume: volumen acumulado con el signo del cambio de precio.

    **Es un acumulado y no es estacionario**: su nivel depende de desde cuándo se
    empezó a sumar. Como feature solo sirve transformado (cambio o z-score),
    nunca crudo.
    """
    c, v = _ohlcv(close, volume)
    cambio = roc(c, 1)
    firmado = np.where(cambio > 0, v, -v)
    firmado[0] = v[0]
    with np.errstate(invalid="ignore"):
        firmado[np.abs(cambio) < np.sqrt(np.finfo(float).eps)] = 0.0
    return np.cumsum(firmado)
