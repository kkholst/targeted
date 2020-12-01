import targeted as tg
import numpy as np
from math import tanh, exp

def test_is_string(caplog):
    '''
    Example test
    '''

    s = "a"
    assert isinstance(s, str), "Input variables should be strings"


def ey(d, a):
    return(np.mean(d[d['a'] == a]['y']))

def test_riskreg1(caplog):
    '''
    riskreg test
    '''

    d = tg.getdata()
    est1 = tg.riskreg(y=d['y'], a=d['a'], type='rd').estimate
    assert len(est1) == 1, "Expecting a single parameter"

    rd = ey(d, 1) - ey(d, 0)
    assert abs(tanh(est1) - rd) < 1e-6

    est2 = tg.riskreg(y=d['y'], a=d['a'], type='rr').estimate
    rr = ey(d, 1) / ey(d, 0)
    assert abs(exp(est2) - rr) < 1e-6
