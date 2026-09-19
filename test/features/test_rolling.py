"""Tests de las primitivas rodantes.

Sin R en la máquina no se puede comparar contra TTR directamente, así que se
valida por tres vías: casos calculados a mano, una transcripción literal de los
bucles en C de TTR contra la que se contrasta la versión compilada, y la
propiedad que más importa en un feature: **no mirar al futuro**.
"""

from __future__ import annotations

import sys
from pathlib import Path

import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from src.features.rolling import (
    ema,
    lag,
    primer_valido,
    roc,
    run_cor,
    run_cov,
    run_mad,
    run_mean,
    run_median,
    run_percent_rank,
    run_sd,
    run_sum,
    run_var,
    wilder_sum,
)

NAN = np.nan


def serie(n=200, semilla=7):
    rng = np.random.default_rng(semilla)
    return 100 * np.exp(np.cumsum(0.01 * rng.standard_normal(n)))


def es_causal(fn, x, cortes=(40, 77, 150)):
    """Calcular sobre un prefijo debe dar lo mismo que el prefijo del total."""
    completo = fn(x)
    for k in cortes:
        np.testing.assert_allclose(completo[:k], fn(x[:k]), equal_nan=True, rtol=1e-12)


# --- transcripción literal de los bucles en C de TTR (src/moving_averages.c,
#     src/wilderSum.c, src/percent_rank.c), sin vectorizar ni compilar ---

def ema_ttr(x, n, wilder):
    razon = 1.0 / n if wilder else 2.0 / (n + 1)
    first = next(i for i, v in enumerate(x) if not np.isnan(v))
    out = [NAN] * len(x)
    seed = 0.0
    for i in range(first, first + n):
        seed += x[i] / n
    out[first + n - 1] = seed
    for i in range(first + n, len(x)):
        out[i] = x[i] * razon + out[i - 1] * (1 - razon)
    return np.array(out)


def wilder_ttr(x, n):
    beg, total, out = n - 1, 0.0, [NAN] * len(x)
    i = 0
    while i < beg:
        if np.isnan(x[i]):
            beg += 1
            i += 1
            continue
        total += x[i]
        i += 1
    out[beg] = x[beg] + total * (n - 1) / n
    for i in range(beg + 1, len(x)):
        out[i] = x[i] + out[i - 1] * (n - 1) / n
    return np.array(out)


def percent_rank_ttr(x, n, mult=0.5):
    out = [NAN] * len(x)
    for i in range(n - 1, len(x)):
        menores = mult
        for j in range(i - n + 1, i):
            d = x[j] - x[i]
            if d < 0:
                menores += 1
            elif abs(d) < 1e-8:
                menores += mult
        out[i] = menores / n
    return np.array(out)


# --- EMA ---

def test_ema_se_siembra_con_la_media_simple():
    """La primera EMA es la media de las primeras n, no el primer valor."""
    np.testing.assert_allclose(ema([1, 2, 3, 4, 5], 3), [NAN, NAN, 2.0, 3.0, 4.0], equal_nan=True)


def test_ema_de_wilder_usa_razon_uno_entre_n():
    np.testing.assert_allclose(ema([1, 2, 3, 4, 5], 3, wilder=True),
                               [NAN, NAN, 2.0, 8 / 3, 31 / 9], equal_nan=True)


@pytest.mark.parametrize("wilder", [False, True])
def test_ema_coincide_con_la_transcripcion_de_ttr(wilder):
    x = serie()
    np.testing.assert_allclose(ema(x, 14, wilder), ema_ttr(x, 14, wilder), equal_nan=True, rtol=1e-12)


def test_ema_salta_los_nan_iniciales():
    x = np.r_[NAN, NAN, 1, 2, 3, 4]
    np.testing.assert_allclose(ema(x, 3), [NAN, NAN, NAN, NAN, 2.0, 3.0], equal_nan=True)


# --- suma de Wilder ---

def test_wilder_sum_a_mano():
    np.testing.assert_allclose(wilder_sum([1, 2, 3, 4], 3), [NAN, NAN, 5.0, 22 / 3], equal_nan=True)


def test_wilder_sum_coincide_con_ttr_incluso_con_nan_inicial():
    x = np.r_[NAN, serie(150)]
    np.testing.assert_allclose(wilder_sum(x, 14), wilder_ttr(x, 14), equal_nan=True, rtol=1e-12)


# --- percent rank ---

def test_percent_rank_nunca_llega_a_cero():
    """El propio valor cuenta como empate: el mínimo de 10 vale 0.05, no 0."""
    x = np.arange(10, 0, -1, dtype=float)
    assert run_percent_rank(x, 10)[-1] == pytest.approx(0.05)


def test_percent_rank_a_mano():
    assert run_percent_rank([1, 2, 3], 3)[-1] == pytest.approx(2.5 / 3)
    assert run_percent_rank([3, 1, 2], 3)[-1] == pytest.approx(0.5)
    assert run_percent_rank([2, 2, 2], 3)[-1] == pytest.approx(0.5)


def test_percent_rank_coincide_con_ttr():
    x = serie()
    np.testing.assert_allclose(run_percent_rank(x, 20), percent_rank_ttr(x, 20), equal_nan=True)


def test_percent_rank_rechaza_multiplicador_invalido():
    with pytest.raises(ValueError):
        run_percent_rank([1, 2, 3], 2, exact_multiplier=1.5)


# --- ventanas simples contra un bucle obvio ---

@pytest.mark.parametrize(("fn", "ref"), [
    (lambda x: run_sum(x, 10), lambda v: v.sum()),
    (lambda x: run_mean(x, 10), lambda v: v.mean()),
    (lambda x: run_var(x, 10), lambda v: v.var(ddof=1)),
    (lambda x: run_sd(x, 10), lambda v: v.std(ddof=1)),
    (lambda x: run_median(x, 10), lambda v: np.median(v)),
    (lambda x: run_mad(x, 10), lambda v: np.median(np.abs(v - np.median(v))) * 1.4826),
])
def test_ventanas_contra_bucle(fn, ref):
    x = serie(60)
    esperado = np.array([NAN] * 9 + [ref(x[i - 9:i + 1]) for i in range(9, 60)])
    np.testing.assert_allclose(fn(x), esperado, equal_nan=True, rtol=1e-12)


def test_varianza_es_muestral_como_en_ttr():
    x = [1.0, 2.0, 3.0, 4.0]
    assert run_var(x, 4)[-1] == pytest.approx(np.var(x, ddof=1))
    assert run_var(x, 4, sample=False)[-1] == pytest.approx(np.var(x))


def test_correlacion_y_covarianza():
    x = serie(80, 1)
    y = 2 * x + 5
    assert run_cor(x, y, 20)[-1] == pytest.approx(1.0)
    assert run_cor(x, -y, 20)[-1] == pytest.approx(-1.0)
    assert run_cov(x, x, 20)[-1] == pytest.approx(run_var(x, 20)[-1])


def test_correlacion_con_serie_constante_es_nan():
    assert np.isnan(run_cor(serie(30), np.ones(30), 10)[-1])


# --- convenciones de NaN ---

def test_relleno_de_nan_al_inicio_de_la_ventana():
    salida = run_mean(np.arange(1.0, 11.0), 4)
    assert np.isnan(salida[:3]).all()
    assert not np.isnan(salida[3:]).any()


def test_nan_intermedio_es_error_de_datos():
    """TTR tolera NaN al inicio; en medio es un hueco en los datos."""
    with pytest.raises(ValueError, match="no son iniciales"):
        run_sum([1.0, 2.0, NAN, 4.0], 2)


def test_ventana_invalida():
    with pytest.raises(ValueError):
        run_sum([1.0, 2.0], 0)


def test_lag_y_roc():
    np.testing.assert_allclose(lag([1, 2, 3], 1), [NAN, 1, 2], equal_nan=True)
    np.testing.assert_allclose(roc([1, np.e, np.e ** 3], 1), [NAN, 1.0, 2.0], equal_nan=True)
    assert primer_valido([NAN, NAN, 3.0]) == 2


# --- lo que más importa: nada mira al futuro ---

@pytest.mark.parametrize("fn", [
    lambda x: ema(x, 14),
    lambda x: ema(x, 14, wilder=True),
    lambda x: wilder_sum(x, 14),
    lambda x: run_percent_rank(x, 20),
    lambda x: run_sd(x, 20),
    lambda x: run_mad(x, 20),
    lambda x: run_median(x, 20),
])
def test_ninguna_primitiva_mira_al_futuro(fn):
    es_causal(fn, serie())
