import numpy as np
import targeted_template.__targeted_template_c__ as targetc

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
    return targetc.expit(val)
