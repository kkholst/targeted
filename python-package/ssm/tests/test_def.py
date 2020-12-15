import ssm
import numpy as np
from math import tanh, exp

def test_is_string(caplog):
    '''
    Example test
    '''

    s = "a"
    assert isinstance(s, str), "Input variables should be strings"
