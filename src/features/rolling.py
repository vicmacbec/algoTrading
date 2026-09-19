"""Primitivas rodantes, compatibles con las convenciones de TTR.

TTR (el paquete de R) es la referencia de las definiciones: sus fórmulas están
probadas por años de uso y documentadas. Lo que no se toma es su código, sino
sus **convenciones**, porque de ellas depende que un feature no mire al futuro:

- **Relleno con NaN al inicio de cada ventana.** Un valor solo existe cuando la
  ventana está completa. Rellenar hacia atrás inyectaría información de velas
  posteriores en las primeras filas de cada serie.
- **Los NaN iniciales se toleran; los intermedios no.** Una serie de precios con
  un hueco en medio es un problema de datos, y se reporta en vez de propagarse.
- **La EMA se siembra con la media simple** de las primeras `n` observaciones,
  no con el primer valor. Es la diferencia entre coincidir con TTR y no hacerlo:
  `ewm_mean` de Polars arranca en el primer dato y da otra serie.

Las recursiones (EMA, suma de Wilder, percent rank) son secuenciales por
naturaleza y se compilan con numba; el resto son ventanas vectorizadas.
"""

from __future__ import annotations

import numpy as np
from numba import njit


def como_array(x) -> np.ndarray:
    """Acepta listas, arrays o Series de Polars y devuelve float64."""
    return np.asarray(x, dtype=np.float64)


def primer_valido(x) -> int:
    """Índice del primer valor no-NaN, o len(x) si no hay ninguno."""
    x = como_array(x)
    idx = np.flatnonzero(~np.isnan(x))
    return int(idx[0]) if idx.size else len(x)


def _validar(x, n: int) -> tuple[np.ndarray, int]:
    """Aplica la regla de TTR: NaN solo al inicio, y una ventana válida."""
    x = como_array(x)
    if n < 1:
        raise ValueError(f"la ventana debe ser >= 1, se pidió {n}")
    f = primer_valido(x)
    if np.isnan(x[f:]).any():
        raise ValueError("la serie tiene NaN que no son iniciales: es un problema de datos")
    return x, f


def _ventanas(x: np.ndarray, n: int, f: int, fn) -> np.ndarray:
    """Aplica `fn` a cada ventana completa y rellena con NaN el resto."""
    salida = np.full(len(x), np.nan)
    cola = x[f:]
    if len(cola) >= n:
        vistas = np.lib.stride_tricks.sliding_window_view(cola, n)
        salida[f + n - 1:] = fn(vistas)
    return salida


def lag(x, k: int = 1) -> np.ndarray:
    """Desplaza k posiciones hacia adelante; las primeras k quedan en NaN."""
    x = como_array(x)
    salida = np.full(len(x), np.nan)
    if k < len(x):
        salida[k:] = x[: len(x) - k]
    return salida


def roc(x, n: int = 1) -> np.ndarray:
    """Cambio porcentual continuo, `log(x_t / x_{t-n})`. Es el tipo por defecto de TTR."""
    x = como_array(x)
    with np.errstate(divide="ignore", invalid="ignore"):
        return np.log(x) - np.log(lag(x, n))


def run_sum(x, n: int) -> np.ndarray:
    x, f = _validar(x, n)
    return _ventanas(x, n, f, lambda v: v.sum(axis=1))


def run_mean(x, n: int) -> np.ndarray:
    x, f = _validar(x, n)
    return _ventanas(x, n, f, lambda v: v.mean(axis=1))


def run_var(x, n: int, sample: bool = True) -> np.ndarray:
    """Varianza rodante. Muestral por defecto (denominador n-1), como TTR."""
    x, f = _validar(x, n)
    ddof = 1 if sample else 0
    if sample and n < 2:
        raise ValueError("la varianza muestral necesita una ventana >= 2")
    return _ventanas(x, n, f, lambda v: v.var(axis=1, ddof=ddof))


def run_sd(x, n: int, sample: bool = True) -> np.ndarray:
    return np.sqrt(run_var(x, n, sample))


def run_median(x, n: int) -> np.ndarray:
    x, f = _validar(x, n)
    return _ventanas(x, n, f, lambda v: np.median(v, axis=1))


def run_mad(x, n: int, constant: float = 1.4826) -> np.ndarray:
    """Desviación absoluta mediana alrededor de la mediana de la misma ventana.

    El factor 1.4826 hace que estime la desviación estándar cuando los datos
    son normales, y a diferencia de ella no se deja arrastrar por un wick.
    """
    x, f = _validar(x, n)

    def mad(v):
        centro = np.median(v, axis=1, keepdims=True)
        return np.median(np.abs(v - centro), axis=1) * constant

    return _ventanas(x, n, f, mad)


def run_cov(x, y, n: int, sample: bool = True) -> np.ndarray:
    x, fx = _validar(x, n)
    y, fy = _validar(y, n)
    if len(x) != len(y):
        raise ValueError("x e y deben tener la misma longitud")
    f = max(fx, fy)
    salida = np.full(len(x), np.nan)
    if len(x) - f >= n:
        vx = np.lib.stride_tricks.sliding_window_view(x[f:], n)
        vy = np.lib.stride_tricks.sliding_window_view(y[f:], n)
        dx = vx - vx.mean(axis=1, keepdims=True)
        dy = vy - vy.mean(axis=1, keepdims=True)
        salida[f + n - 1:] = (dx * dy).sum(axis=1) / (n - 1 if sample else n)
    return salida


def run_cor(x, y, n: int, sample: bool = True) -> np.ndarray:
    """Correlación de Pearson rodante. NaN donde alguna serie es constante."""
    with np.errstate(divide="ignore", invalid="ignore"):
        return run_cov(x, y, n, sample) / (run_sd(x, n, sample) * run_sd(y, n, sample))


@njit(cache=True)
def _ema_nucleo(x, n, razon, primero):
    salida = np.full(len(x), np.nan)
    semilla = 0.0
    for i in range(primero, primero + n):
        semilla += x[i] / n
    salida[primero + n - 1] = semilla
    for i in range(primero + n, len(x)):
        salida[i] = x[i] * razon + salida[i - 1] * (1.0 - razon)
    return salida


def ema(x, n: int, wilder: bool = False) -> np.ndarray:
    """Media móvil exponencial con la siembra de TTR.

    La razón es `2/(n+1)`, o `1/n` si `wilder=True`, que es la suavización que
    usan ATR y ADX. La primera observación válida es la media simple de las
    primeras `n`, en la posición `n-1`; antes, NaN.
    """
    x, f = _validar(x, n)
    if len(x) - f < n:
        raise ValueError(f"hacen falta al menos {n} valores no-NaN y hay {len(x) - f}")
    razon = 1.0 / n if wilder else 2.0 / (n + 1)
    return _ema_nucleo(x, n, razon, f)


@njit(cache=True)
def _wilder_nucleo(x, n, primero):
    salida = np.full(len(x), np.nan)
    inicio = primero + n - 1
    suma = 0.0
    for i in range(primero, inicio):
        suma += x[i]
    salida[inicio] = x[inicio] + suma * (n - 1) / n
    for i in range(inicio + 1, len(x)):
        salida[i] = x[i] + salida[i - 1] * (n - 1) / n
    return salida


def wilder_sum(x, n: int) -> np.ndarray:
    """Suma suavizada de Wilder, la base de los indicadores direccionales (ADX)."""
    x, f = _validar(x, n)
    if len(x) - f < n:
        raise ValueError(f"hacen falta al menos {n} valores no-NaN y hay {len(x) - f}")
    return _wilder_nucleo(x, n, f)


@njit(cache=True)
def _percent_rank_nucleo(x, n, mult, primero):
    salida = np.full(len(x), np.nan)
    for i in range(primero + n - 1, len(x)):
        menores = mult  # el propio valor cuenta como empate consigo mismo
        for j in range(i - n + 1, i):
            dif = x[j] - x[i]
            if dif < 0.0:
                menores += 1.0
            elif abs(dif) < 1e-8:
                menores += mult
        salida[i] = menores / n
    return salida


def run_percent_rank(x, n: int, exact_multiplier: float = 0.5) -> np.ndarray:
    """Percentil del valor actual dentro de su ventana, en (0, 1].

    Cuenta los valores menores y suma `exact_multiplier` por cada empate,
    **incluido el propio valor consigo mismo**. Por eso nunca devuelve 0: el
    mínimo de una ventana de 10 sin empates vale 0.05. Es la normalización que
    hace comparables series con escalas distintas sin mirar fuera de la ventana.
    """
    if not 0.0 <= exact_multiplier <= 1.0:
        raise ValueError("exact_multiplier debe estar en [0, 1]")
    x, f = _validar(x, n)
    if n == 1:
        salida = np.full(len(x), exact_multiplier)
        salida[:f] = np.nan
        return salida
    return _percent_rank_nucleo(x, n, exact_multiplier, f)
