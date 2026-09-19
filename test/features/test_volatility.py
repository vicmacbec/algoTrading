"""Tests de los estimadores de volatilidad.

Se validan contra formas cerradas: con velas de geometría constante cada
estimador tiene un valor exacto conocido, y los que usan OHLC deben coincidir
entre sí en los casos donde sus supuestos se cumplen.
"""

from __future__ import annotations

import math
import sys
from pathlib import Path

import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from src.features.rolling import lag, run_var
from src.features.volatility import (
    PERIODOS_POR_ANIO,
    vol_close,
    vol_garman_klass,
    vol_gk_yz,
    vol_parkinson,
    vol_rogers_satchell,
    vol_yang_zhang,
)

N = 365


def ohlc(n=200, semilla=3):
    rng = np.random.default_rng(semilla)
    c = 100 * np.exp(np.cumsum(0.01 * rng.standard_normal(n)))
    o = np.r_[c[0], c[:-1] * np.exp(0.002 * rng.standard_normal(n - 1))]
    h = np.maximum(o, c) * np.exp(np.abs(0.005 * rng.standard_normal(n)))
    l = np.minimum(o, c) * np.exp(-np.abs(0.005 * rng.standard_normal(n)))
    return o, h, l, c


def doji(n=30, medio=100.0, b=0.02):
    """Velas sin cuerpo (apertura = cierre) y con mechas simétricas."""
    m = np.full(n, medio)
    return m, m * math.exp(b), m * math.exp(-b), m, b


# --- formas cerradas ---

def test_parkinson_con_rango_constante():
    h, l = np.full(30, 110.0), np.full(30, 100.0)
    esperado = math.sqrt(N / (4 * math.log(2))) * math.log(1.1)
    assert vol_parkinson(h, l, 10, N)[-1] == pytest.approx(esperado)


def test_en_velas_doji_garman_klass_y_rogers_satchell_coinciden():
    """Sin cuerpo ni deriva, ambos estimadores valen sqrt(2N)·b."""
    o, h, l, c, b = doji()
    esperado = math.sqrt(2 * N) * b
    assert vol_garman_klass(o, h, l, c, 10, N)[-1] == pytest.approx(esperado)
    assert vol_rogers_satchell(o, h, l, c, 10, N)[-1] == pytest.approx(esperado)


def test_precios_constantes_dan_volatilidad_cero():
    p = np.full(40, 100.0)
    for fn in (vol_garman_klass, vol_rogers_satchell, vol_gk_yz, vol_yang_zhang):
        assert fn(p, p, p, p, 10, N)[-1] == pytest.approx(0.0)
    assert vol_parkinson(p, p, 10, N)[-1] == pytest.approx(0.0)
    assert vol_close(p, 10, N)[-1] == pytest.approx(0.0)


def test_vol_close_es_la_desviacion_de_retornos_log():
    _, _, _, c = ohlc()
    r = np.log(c[1:] / c[:-1])
    esperado = math.sqrt(N) * np.std(r[-9:], ddof=1)   # n=10 precios, 9 retornos
    assert vol_close(c, 10, N)[-1] == pytest.approx(esperado)


def test_vol_close_con_media_cero():
    _, _, _, c = ohlc()
    r = np.log(c[1:] / c[:-1])
    esperado = math.sqrt(N) * math.sqrt(np.sum(r[-9:] ** 2) / 8)
    assert vol_close(c, 10, N, mean0=True)[-1] == pytest.approx(esperado)


def test_yang_zhang_se_arma_con_sus_componentes():
    o, h, l, c = ohlc()
    n, alpha = 10, 1.34
    k = (alpha - 1) / (alpha + (n + 1) / (n - 1))
    s2o = N * run_var(np.log(o / lag(c)), n)
    s2c = N * run_var(np.log(c / o), n)
    s2rs = vol_rogers_satchell(o, h, l, c, n, N) ** 2
    np.testing.assert_allclose(vol_yang_zhang(o, h, l, c, n, N),
                               np.sqrt(s2o + k * s2c + (1 - k) * s2rs), equal_nan=True)


def test_la_escala_es_monotona_en_n():
    """Cambiar N solo reescala: no altera el orden, que es lo que ve el modelo."""
    o, h, l, c = ohlc()
    a = vol_yang_zhang(o, h, l, c, 10, 365)
    b = vol_yang_zhang(o, h, l, c, 10, 2190)
    np.testing.assert_allclose(b, a * math.sqrt(2190 / 365), equal_nan=True)


# --- dónde aparece el primer valor ---

@pytest.mark.parametrize(("fn", "primero"), [
    (lambda o, h, l, c: vol_parkinson(h, l, 10, N), 9),
    (lambda o, h, l, c: vol_garman_klass(o, h, l, c, 10, N), 9),
    (lambda o, h, l, c: vol_rogers_satchell(o, h, l, c, 10, N), 9),
    (lambda o, h, l, c: vol_close(c, 10, N), 9),
    (lambda o, h, l, c: vol_gk_yz(o, h, l, c, 10, N), 10),     # necesita el cierre previo
    (lambda o, h, l, c: vol_yang_zhang(o, h, l, c, 10, N), 10),
])
def test_primer_valor_valido(fn, primero):
    salida = fn(*ohlc())
    assert np.isnan(salida[:primero]).all()
    assert not np.isnan(salida[primero:]).any()


# --- validación y causalidad ---

def test_precios_no_positivos_son_error_de_datos():
    p = np.full(20, 100.0)
    malo = p.copy()
    malo[5] = 0.0
    with pytest.raises(ValueError, match="<= 0"):
        vol_parkinson(p, malo, 5, N)


def test_periodos_por_anio_para_mercado_continuo():
    assert PERIODOS_POR_ANIO["4h"] == 2190
    assert PERIODOS_POR_ANIO["1d"] == 365


@pytest.mark.parametrize("fn", [
    lambda o, h, l, c: vol_parkinson(h, l, 10, N),
    lambda o, h, l, c: vol_garman_klass(o, h, l, c, 10, N),
    lambda o, h, l, c: vol_rogers_satchell(o, h, l, c, 10, N),
    lambda o, h, l, c: vol_gk_yz(o, h, l, c, 10, N),
    lambda o, h, l, c: vol_yang_zhang(o, h, l, c, 10, N),
    lambda o, h, l, c: vol_close(c, 10, N),
])
def test_ningun_estimador_mira_al_futuro(fn):
    o, h, l, c = ohlc()
    completo = fn(o, h, l, c)
    for k in (40, 90, 150):
        np.testing.assert_allclose(completo[:k], fn(o[:k], h[:k], l[:k], c[:k]),
                                   equal_nan=True, rtol=1e-12)
