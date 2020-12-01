import numpy as np
import numpy.linalg as npl
import statsmodels.api as sm
import targeted.__targeted_c__ as targetc


def iid(x):
    """Returns the estimated influence function (iid decomposition)

    Parameters
    ----------
    x: statsmodels.GLM

    Returns
    -------
    numpy.array

    """
    if not isinstance(x, sm.GLM):
        raise TypeError("Expected a 'statsmodels.GLM' object")
    beta = x.fit().params
    H = x.hessian(beta)
    U = x.score_obs(beta)
    return (npl.pinv(H) @ U.transpose()).transpose()

def tcrossprod(x):
    """Return crossprod x'x

    Parameters
    ----------
    x: numpy.array

    Returns
    -------
    numpy.array

    """

    if not isinstance(x, (list, tuple, np.ndarray)):
        raise TypeError("Expected a numpy array (matrix) object")
    return x.transpose() @ x

def robustse(x):
    """Robust standard errors

    Parameters
    ----------
    x: statsmodels.GLM

    Returns
    -------
    numpy.array
       matrix with standard errors

    """

    if not isinstance(x, sm.GLM):
        raise TypeError("Expected a 'statsmodels.GLM' object")
    return np.array(tcrossprod(iid(x)).diagonal())**.5


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

#    print(x)
    val = np.array(x)
#    print(val)
#    print(np.array(x))
    return targetc.expit(val)
