"""Tests de ATR, ADX, CLV, CMF, MFI y OBV.

Además de casos a mano y de los rangos de cada indicador, se prueba la
desviación documentada respecto a TTR —un mercado plano da 0, no NaN— y que
ninguno mira al futuro.
"""

from __future__ import annotations

import sys
from pathlib import Path

import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from src.features.indicators import adx, atr, clv, cmf, mfi, obv, true_range
from src.features.rolling import ema

NAN = np.nan


def ohlcv(n=200, semilla=11):
    rng = np.random.default_rng(semilla)
    c = 100 * np.exp(np.cumsum(0.01 * rng.standard_normal(n)))
    o = np.r_[c[0], c[:-1] * np.exp(0.002 * rng.standard_normal(n - 1))]
    h = np.maximum(o, c) * np.exp(np.abs(0.005 * rng.standard_normal(n)))
    l = np.minimum(o, c) * np.exp(-np.abs(0.005 * rng.standard_normal(n)))
    v = 1000 * np.exp(rng.standard_normal(n))
    return o, h, l, c, v


def tendencia_alcista(n=60):
    c = 100.0 + np.arange(n)
    return c + 0.5, c - 0.5, c


# --- rango verdadero y ATR ---

def test_true_range_a_mano():
    tr, alto, _ = true_range([10, 12, 11], [8, 9, 7], [9, 11, 10])
    np.testing.assert_allclose(tr, [NAN, 3.0, 4.0], equal_nan=True)
    np.testing.assert_allclose(alto, [NAN, 12.0, 11.0], equal_nan=True)


def test_true_range_incluye_el_hueco_de_apertura():
    """Un gap alcista entre velas es rango aunque no esté dentro de ninguna."""
    tr, _, _ = true_range([10, 15], [9, 14], [9.5, 14.5])
    assert tr[1] == pytest.approx(15 - 9.5)


def test_atr_empieza_una_posicion_despues_por_el_primer_tr_nan():
    _, h, l, c, _ = ohlcv()
    salida = atr(h, l, c, 14)
    assert np.isnan(salida[:14]).all() and not np.isnan(salida[14])


def test_atr_es_la_ema_de_wilder_del_rango_verdadero():
    _, h, l, c, _ = ohlcv()
    tr, _, _ = true_range(h, l, c)
    np.testing.assert_allclose(atr(h, l, c, 14), ema(tr, 14, wilder=True), equal_nan=True)


# --- ADX / DI ---

def test_adx_aparece_en_la_posicion_2n_menos_1():
    _, h, l, c, _ = ohlcv()
    salida = adx(h, l, c, 14)["adx"]
    assert np.isnan(salida[:27]).all() and not np.isnan(salida[27])


def test_en_tendencia_pura_domina_di_positivo():
    h, l, c = tendencia_alcista()
    r = adx(h, l, c, 14)
    assert np.nanmin(r["di_pos"]) > 0
    assert np.nanmax(r["di_neg"]) == pytest.approx(0.0)
    assert r["adx"][-1] == pytest.approx(100.0)


def test_adx_y_di_estan_entre_0_y_100():
    _, h, l, c, _ = ohlcv()
    for nombre, valores in adx(h, l, c, 14).items():
        v = valores[~np.isnan(valores)]
        assert (v >= 0).all() and (v <= 100 + 1e-9).all(), nombre


def test_mercado_plano_da_cero_y_no_nan():
    """Desviación documentada de TTR: allí 0/0 contamina la serie entera."""
    p = np.full(60, 100.0)
    r = adx(p, p, p, 14)
    assert r["adx"][-1] == 0.0
    assert not np.isnan(r["adx"][27:]).any()


# --- CLV, CMF, MFI, OBV ---

def test_clv_marca_la_posicion_del_cierre():
    np.testing.assert_allclose(clv([10, 10, 10], [0, 0, 0], [10, 0, 5]), [1.0, -1.0, 0.0])


def test_clv_de_vela_sin_rango_vale_cero():
    assert clv([5.0], [5.0], [5.0])[0] == 0.0


def test_cmf_esta_entre_menos_1_y_1():
    _, h, l, c, v = ohlcv()
    salida = cmf(h, l, c, v, 20)
    valores = salida[~np.isnan(salida)]
    assert (np.abs(valores) <= 1 + 1e-12).all()
    assert np.isnan(salida[:19]).all()


def test_mfi_esta_entre_0_y_100():
    _, h, l, c, v = ohlcv()
    valores = mfi(h, l, c, v, 14)
    valores = valores[~np.isnan(valores)]
    assert (valores >= 0).all() and (valores <= 100).all()


def test_mfi_solo_flujo_positivo_da_100():
    h, l, c = tendencia_alcista(30)
    assert mfi(h, l, c, np.ones(30), 14)[-1] == 100.0


def test_mfi_sin_flujo_en_ninguna_direccion_da_50():
    p = np.full(30, 100.0)
    assert mfi(p, p, p, np.ones(30), 14)[-1] == 50.0


def test_obv_suma_con_el_signo_del_precio():
    np.testing.assert_allclose(obv([10, 11, 10.5, 10.5], [5, 3, 2, 7]), [5, 8, 6, 6])


def test_obv_arranca_con_el_primer_volumen():
    assert obv([10, 11], [5, 3])[0] == 5


# --- causalidad ---

@pytest.mark.parametrize("fn", [
    lambda o, h, l, c, v: atr(h, l, c, 14),
    lambda o, h, l, c, v: adx(h, l, c, 14)["adx"],
    lambda o, h, l, c, v: adx(h, l, c, 14)["di_pos"],
    lambda o, h, l, c, v: cmf(h, l, c, v, 20),
    lambda o, h, l, c, v: mfi(h, l, c, v, 14),
    lambda o, h, l, c, v: obv(c, v),
])
def test_ningun_indicador_mira_al_futuro(fn):
    series = ohlcv()
    completo = fn(*series)
    for k in (60, 110, 170):
        np.testing.assert_allclose(completo[:k], fn(*(s[:k] for s in series)),
                                   equal_nan=True, rtol=1e-12)
