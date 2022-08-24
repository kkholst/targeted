import numpy as np
import netadp.__netadp_c__ as netadpc

def add1(x):
    return netadpc.myloop(x)

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
    return netadpc.expit(val)
