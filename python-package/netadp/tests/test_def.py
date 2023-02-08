from math import tanh, exp

def test_is_string(caplog):
    '''
    Example test
    '''

    s = "a"
    assert isinstance(s, str), "Input variables should be strings"


def test1(caplog):
    '''
    Example test
    '''

    assert 0 < 1e-6
