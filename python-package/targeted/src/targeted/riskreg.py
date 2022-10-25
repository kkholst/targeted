# -*- coding: utf-8 -*-
#
# Copyright (c) 2019-2022 Klaus K. Holst.  All rights reserved.

import targeted as tg
import numpy as np
import statsmodels.api as sm
from scipy import optimize

class riskreg:
    """Documentation for Riskreg

    """

    model = None
    propensity = None
    mle = None
    propensity_coef = None
    mle_coef = None
    modeltype = None
    estimate = None

    def __stageone(self, pr):
        pr = pr.flatten()
        n = pr.size
        par = np.concatenate((self.mle_coef, self.propensity_coef))
        self.model.update(par)

        pp = self.model.pr()
        p0 = pp[:, 1].flatten()
        p1 = pp[:, 2].flatten()
        targ = pp[:, 3].flatten()
        if self.modeltype == 'rd':
            # E(A drho/(Pa(1-Pa))|V) = pr*drho/[p1(1-p1)]
            nom = pr*(1-targ*targ)/(p1*(1-p1))
            # E(1/(Pa(1-Pa))|V) =  (1-pr)/[p0(1-p0)] + pr/[p1(1-p1)]
            denom = (1-pr)/(p0*(1-p0)) + pr/(p1*(1-p1))
            omega = nom/denom / (pr*p0*(1-p0))
        else:  # rr
            # E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            nom = pr*p1/(1-p1)
            # E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            denom = (1-pr)*p0/(1-p0) + pr*p1/(1-p1)
            omega = nom/denom / (pr*(1-p0))

        w = self.model.data(tg.datatype.w)
        w = np.multiply(w.flatten(), omega).reshape(n, 1)
        self.model.weights(w)
        self.model.update(par)

    def __init__(self, y, a, **kwargs):
        """Risk regression with binary exposure

        Examples
        --------

        Parameters
        ----------
        y: list or numpy.array
            Response vector (0,1)
        a: list or numpy.array
            Exposure vector (0,1)
        x1: numpy.array, optional
            Design matrix for linear interactions with exposure 'a'
        x2: numpy.array, optional
            Design matrix for nuisance parameter regression (odds-product)
        x3: numpy.array, optional
            Design matrix for propoensity model
        type: str
            Relative risk: 'rr', Risk difference: 'rd'

        Returns
        -------
        Riskreg
            Riskreg object

        """

        n = y.size
        one = np.repeat(1.0, n).reshape(n,1)
        x1 = kwargs.get('x1', one)
        x2 = kwargs.get('x2', one)
        x3 = kwargs.get('x3', one)
        w = kwargs.get('weights', one)
        self.modeltype = kwargs.get('type', 'rr')
        self.model = tg.riskregmodel(y, a, x1, x2, x3, w, self.modeltype)
        self.propensity = sm.GLM(endog=a, exog=x3, freq_weights=w.squeeze(),
                                 family=sm.families.Binomial())
        self.propensity_coef = self.propensity.fit().params
        self.mle = riskreg_mle(y, a, x2=x2, x1=x1, weights=w, type=self.modeltype)
        self.mle_coef = self.mle['x']

        pr = tg.expit(np.matmul(self.model.data(tg.datatype.x3), self.propensity_coef))
        self.__stageone(pr)
        alpha0 = self.mle_coef[:x1.shape[1]].reshape(x1.shape[1], 1)

        def obj(alpha):
            return sum(sum(self.model.esteq(alpha, pr))**2)

        op = optimize.minimize(obj, alpha0, method='Nelder-Mead')
        self.estimate = op['x']

    def __repr__(self):
        return "Riskreg. Estimate: " + str(self.estimate)

    def __str__(self):
        return "Riskreg. Estimate: " + str(self.estimate)


def riskreg_mle(y, a, x2, *args, **kwargs):
    """Maximum Likelihood estimation of risk-regression model

    The parameter of interest is either a risk difference or relative
    risk parameter with the effect of confounders decsribed on a log
    odds-product scale.

    Parameters
    ----------
    y: list or numpy.array
        Response vector (0,1)
    a: list or numpy.array
        Exposure vector (0,1)
    x2: numpy.array
        Design matrix for nuisance parameter regression (odds-product)
    x1: numpy.array, optional
        Design matrix for linear interactions with exposure 'a'
    w: list or numpy.array, optional
        Weights vector
    type: str
        Relative risk: ``rr``, Risk difference: ``rd``

    Returns
    -------
    Riskreg
        Riskreg object

    References
    ----------
    Details behind the method can be found in [1]_.

    .. [1] Richardson, T. S., Robins, J. M., & Wang, L. (2017). On modeling and
       estimation for the relative risk and risk difference. Journal of the
       American Statistical Association, 112(519),
       1121–1130. http://dx.doi.org/10.1080/01621459.2016.1192546
    """

    one = np.repeat(1.0, len(y)).reshape(len(y), 1)
    x1 = kwargs.get('x1', one)
    w = kwargs.get('weights', one)
    modeltype = kwargs.get('type', 'rr')
    m = tg.riskregmodel(y, a, x1, x2, one, w, modeltype)

    def obj(theta):
        m.update(theta.reshape(theta.size, 1))
        return -m.loglik()

    def jac(theta):
        m.update(theta.reshape(theta.size, 1))
        return -m.score().flatten()

    p = x1.shape[1]+x2.shape[1]
    init = kwargs.get('init', np.repeat(0, p))
    op = optimize.minimize(obj, init, method='BFGS', jac=jac)
    if not op['success']:
        op = optimize.minimize(obj, init, method='CG', jac=jac)
    if not op['success']:
        op = optimize.minimize(obj, init, method='Nelder-Mead', jac=jac)
    if not op['success']:
        op = optimize.minimize(obj, init, method='TNC', jac=jac)

    return op
