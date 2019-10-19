import target.target_c as tc
import numpy as np

def expit(x):
    """Sigmoid function (Inverse Logit)

    :param x: numpy matrix/array
    :returns: Inverse logit
    :rtype: numpy array

    """
    return tc.expit(np.matrix(x))
