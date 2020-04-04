import targeted.__targeted_c__ as targetc
import numpy as np
import numpy.linalg as npl
import statsmodels.api as sm

def iid(x):
    """Returns the estimated influence function (iid decomposition)

    :param x: statsmodels.GLM object
    :returns: matrix
    :rtype: numpy.matirx

    """
    if not isinstance(x, sm.GLM):
        raise TypeError("Expected a 'statsmodels.GLM' object")
    beta = x.fit().params
    H = x.hessian(beta)
    U = x.score_obs(beta)
    return np.matmul(npl.pinv(H), U.transpose()).transpose()

def tcrossprod(x):
    """Return crossprod x'x

    :param x: matrix
    :returns: matrix
    :rtype: numpy.matrix

    """
    if not isinstance(x, (list, tuple, np.ndarray)):
        raise TypeError("Expected a numpy array (matrix) object")
    return np.matrix(np.matmul(x.transpose(), x))

def robustse(x):
    """Robust standard errors

    :param x: statsmodels.GLM object
    :returns: array with standard errors
    :rtype: numpy.array

    """
    if not isinstance(x, sm.GLM):
        raise TypeError("Expected a 'statsmodels.GLM' object")
    return np.array(tcrossprod(iid(x)).diagonal())**.5


def expit(x):
    """Sigmoid function (Inverse Logit)

    :param x: numpy matrix/array
    :returns: Inverse logit
    :rtype: numpy array

    """
    return targetc.expit(np.matrix(x))
