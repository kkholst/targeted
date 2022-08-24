import numpy as np
import targeted_template.__targeted_template_c__ as cpp

def add1(x):
    return cpp.myloop(x)

def add(x,y):
    return cpp.add(x,y)

def scale2(x):
    return cpp.scale2(x)

def expit(x):
    """Sigmoid function (Inverse Logit)

    Parameters
    ----------
    x: numpy.array

    Returns
    -------
    numpy.array
        matrix with elementwise inverse logit

    """

    val = np.array(x)
    return cpp.expit(val)
