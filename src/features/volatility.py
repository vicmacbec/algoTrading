"""Estimadores de volatilidad, con las fórmulas de `TTR::volatility`.

El estimador clásico solo mira el cierre de cada vela y tira la información del
máximo y el mínimo. Los que usan OHLC completo son **estadísticamente más
eficientes**: con la misma ventana estiman la volatilidad con menos error, o con
el mismo error usando menos velas, que es justo lo que importa cuando el
régimen cambia rápido.

| Estimador | Usa | Supuesto que rompe |
|---|---|---|
| `close` | cierres | ninguno, pero desperdicia información |
| `parkinson` | máximo y mínimo | asume sin deriva y sin saltos entre velas |
| `garman_klass` | OHLC | asume sin deriva |
| `rogers_satchell` | OHLC | robusto a la deriva (tendencia) |
| `gk_yz` | OHLC + cierre previo | incorpora el salto de apertura |
| `yang_zhang` | OHLC + cierre previo | robusto a deriva y a saltos; el más completo |

**Anualización.** TTR usa `N=260` por defecto, que son los días hábiles de las
acciones. Cripto opera 24/7, así que aquí `N` depende del intervalo; ver
`PERIODOS_POR_ANIO`. Como feature de un modelo la escala da igual —cualquier
constante es una transformación monótona—, pero para leer el número importa.
"""

from __future__ import annotations

import math

import numpy as np

from src.features.rolling import como_array, lag, roc, run_sd, run_sum, run_var

# Velas por año en un mercado que no cierra.
PERIODOS_POR_ANIO = {"1h": 365 * 24, "4h": 365 * 6, "1d": 365, "1w": 52}
N_TTR_ACCIONES = 260

_K_GK = 2.0 * math.log(2.0) - 1.0


def _precios(*series) -> tuple[np.ndarray, ...]:
    """Valida que los precios sean positivos: el logaritmo de 0 no existe."""
    arrays = tuple(como_array(s) for s in series)
    for a in arrays:
        if np.any(a[~np.isnan(a)] <= 0):
            raise ValueError("hay precios <= 0: es un problema de datos, no de volatilidad")
    if len({len(a) for a in arrays}) != 1:
        raise ValueError("las series de precios deben tener la misma longitud")
    return arrays


def vol_close(close, n: int = 10, N: int = 365, mean0: bool = False) -> np.ndarray:
    """Desviación estándar de los retornos logarítmicos, anualizada.

    Usa una ventana de `n-1` retornos porque `n` precios dan `n-1` cambios.
    `mean0=True` asume media cero, que es más estable en ventanas cortas.
    """
    (close,) = _precios(close)
    r = roc(close, 1)
    if mean0:
        if n < 3:
            raise ValueError("mean0 necesita n >= 3")
        return np.sqrt(N) * np.sqrt(run_sum(r ** 2, n - 1) / (n - 2))
    return np.sqrt(N) * run_sd(r, n - 1)


def vol_parkinson(high, low, n: int = 10, N: int = 365) -> np.ndarray:
    h, l = _precios(high, low)
    return np.sqrt(N / (4.0 * n * math.log(2.0)) * run_sum(np.log(h / l) ** 2, n))


def vol_garman_klass(open_, high, low, close, n: int = 10, N: int = 365) -> np.ndarray:
    o, h, l, c = _precios(open_, high, low, close)
    termino = 0.5 * np.log(h / l) ** 2 - _K_GK * np.log(c / o) ** 2
    return np.sqrt(N / n * run_sum(termino, n))


def vol_rogers_satchell(open_, high, low, close, n: int = 10, N: int = 365) -> np.ndarray:
    o, h, l, c = _precios(open_, high, low, close)
    termino = np.log(h / c) * np.log(h / o) + np.log(l / c) * np.log(l / o)
    return np.sqrt(N / n * run_sum(termino, n))


def vol_gk_yz(open_, high, low, close, n: int = 10, N: int = 365) -> np.ndarray:
    """Garman-Klass con el salto de apertura respecto al cierre previo."""
    o, h, l, c = _precios(open_, high, low, close)
    c1 = lag(c, 1)
    with np.errstate(invalid="ignore"):
        termino = (np.log(o / c1) ** 2 + 0.5 * np.log(h / l) ** 2
                   - _K_GK * np.log(c / o) ** 2)
    return np.sqrt(N / n * run_sum(termino, n))


def vol_yang_zhang(open_, high, low, close, n: int = 10, N: int = 365,
                   alpha: float = 1.34) -> np.ndarray:
    """Combina la varianza de apertura, la de cierre y la de Rogers-Satchell.

    `k` pondera los componentes para minimizar el error del estimador; con el
    `alpha` de 1.34 que propusieron Yang y Zhang, es el de TTR.
    """
    if n < 2:
        raise ValueError("yang_zhang necesita n >= 2")
    o, h, l, c = _precios(open_, high, low, close)
    k = (alpha - 1.0) / (alpha + (n + 1.0) / (n - 1.0))
    c1 = lag(c, 1)
    with np.errstate(invalid="ignore"):
        s2o = N * run_var(np.log(o / c1), n)
    s2c = N * run_var(np.log(c / o), n)
    s2rs = vol_rogers_satchell(o, h, l, c, n, N) ** 2
    return np.sqrt(s2o + k * s2c + (1.0 - k) * s2rs)
