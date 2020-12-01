import pkg_resources
import pandas as pd
import numpy as np
import targeted as tg


def getdata(dataset:str='d', list:bool=False):
    """Load example data from the 'targeted' package

    Parameters
    ----------
    filename: str
       Name of the filename of the data
    list: bool

    Examples
    --------
    >>> # Surgical unit data
    >>> targeted.getdata('surgunit').head()
       bloodclot  prognostic  enzyme  liver function  survival
    0        6.7          62      81            2.59       200
    1        5.1          59      66            1.70       101
    2        7.4          57      83            2.16       204
    3        6.5          73      41            2.01       101
    4        7.8          65     115            4.30       509
    >>> # Example data for risk-regression model
    >>> targeted.getdata().head()
       y  a         x         z
    0  0  0 -0.626454  1.134965
    1  0  0  0.183643  1.111932
    2  0  0 -0.835629 -0.870778
    3  1  0  1.595281  0.210732
    4  1  1  0.329508  0.069396

    """
    if list:
        return(pkg_resources.resource_listdir('targeted','data'))
    filename='data/' + dataset + '.csv.gz'
    inp = pkg_resources.resource_filename('targeted', filename)
    data = pd.read_csv(inp, sep=',', header=0)
    return(data)


def sim_bin(n=1e2,
          exposure=1,
          binary=True,
          p=1,
          rho=0.5,
          gamma=1.0,
          outcome_intercept=-1.0,
          exposure_intercept=-1.0):
    """Simulate data from linear model or logistic regression model with binary exposure

    Parameters
    ----------
    n: int
       number of samples
    exposure: float
       direct exposure effect (linear predictor scale)
    p: int
       number of covariates/auxiliary variables
    rho: float
       correlation between covariates
    gamma: float
       effect of covariates on exposure (linear predictor scale)
    outcome_intercept: float
       intercept of outcome model
    exposure_intercept: float
       intercept of exposure model

    Returns
    -------
    pandas.DataFrame
       DataFrame with: response ``y``, exposure ``a``, and covariates ``x1``, ``x2``, ...

    Examples
    --------
    >>> import targeted
    >>> from numpy import mean
    >>> d = targeted.sim_bin(n=1e4, gamma=0)
    >>> ey = lambda x: mean(d[d['a']==x]['y'])
    >>> ey(1)-ey(0)  # Causal effect
    -0.1973030984022366  # random

    Notes
    -----
    The model for the outcome is given by

    .. math:: g(\\mathbb{E}[Y\\mid A, X]) = \\mu_Y + \\theta A + \\beta^T X

    where :math:`g` is either the identify function (``binary=False``)
    or the logit function (``binary=True``). The exposure effect
    :math:`\\theta` is controlled by the argument ``exposure``.

    The covariates are :math:`X\\sim\\mathcal{N}_p(0, \\Sigma)` where
    the covariance matrix :math:`\\Sigma` is a compound symmetry
    matrix with correlation :math:`\\rho` controlled by the argument
    ``rho``.

    For the exposure the model is given by

    .. math:: \\operatorname{logit}(\\mathbb{E}[A\\mid X]) = \\mu_A + \\gamma^T X

    The intercept terms :math:`\\mu_Y` and :math:`\\mu_A` are
    controlled by the arguments ``outcome_intercept`` and
    ``exposure_intercept``.

    """

    S = np.diag([1-rho] * int(p)) + rho
    L = np.linalg.cholesky(S)
    x = np.random.normal(size=int(p*n)).reshape(int(n), int(p)) @ L.T
    beta = [1]*int(p)
    gamma = [gamma]*int(p)
    lp_exposure = exposure_intercept + x @ gamma
    pr = tg.expit(lp_exposure)
    a = (np.random.uniform(size=int(n)) > pr)*1
    outcome = outcome_intercept + a*exposure + x @ beta
    if binary:
        pr = tg.expit(outcome)
        outcome = (np.random.uniform(size=int(n)) > pr)

    df = pd.DataFrame.from_records(np.hstack((outcome.T, a.T, x)))
    cnames = ['y', 'a'] + list(map(lambda x: 'x' + str(x+1), range(p)))
    cdict = {i: cnames[i] for i in range(0, len(cnames))}
    df = df.rename(columns=cdict)

    return(df)
