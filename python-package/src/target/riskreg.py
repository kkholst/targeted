import target as tg
import numpy as np
import statsmodels.api as sm
from scipy import optimize

def riskreg_mle(y, a, x2, *args, **kwargs):
    one = np.matrix(np.repeat(1.0, len(y))).transpose()
    x1 = kwargs.get('x1', one)
    w = kwargs.get('weights', one)
    model = kwargs.get('model', 'rr')
    m = tg.riskregmodel(y, a, x1, x2, one, w, model)

    def obj(theta):
        m.update(np.matrix(theta))
        return -m.loglik()

    def jac(theta):
        m.update(np.matrix(theta))
        return -m.score().flatten()

    p = x1.shape[1]+x2.shape[1]
    init = kwargs.get('init', np.repeat(0, p))
    op = optimize.minimize(obj, init, method='BFGS', jac=jac)
    return op

def riskreg(y, a, optimal=True, *args, **kwargs):
    """Risk regression with binary exposure

    :param y: Response vector (0,1)
    :param a: Exposure vector (0,1)
    :param x1: Design matrix for linear interactions with exposure 'a'
    :param x2: Design matrix for nuisance parameter (odds-product)
    :param x3: Design matrix for propoensity modle
    :returns: list with parameter estimates
    :rtype: list

    """
    n = len(y)
    one = np.matrix(np.repeat(1.0, len(y))).transpose()
    x1 = kwargs.get('x1', one)
    x2 = kwargs.get('x2', one)
    x3 = kwargs.get('x3', one)
    w = kwargs.get('weights', one)
    model = kwargs.get('model', 'rr')

    prop_mod = sm.GLM(endog=a, exog=x3, family=sm.families.Binomial())
    b1 = prop_mod.fit().params
    mle = riskreg_mle(y, a, x2=x2, x1=x1, weights=w, model=model)
    mle_coef = mle['x']

    lp = np.matmul(x3, b1)
    pr = tg.expit(lp)
    m = tg.riskregmodel(y, a, x1, x2, x3, w, model)
    par = np.concatenate((mle_coef, b1))
    m.update(par)

    if optimal:
        pp = m.pr()
        p0 = pp[:, 1].flatten()
        p1 = pp[:, 2].flatten()
        targ = pp[:, 3].flatten()
        if model == 'rd':
            #            E(A drho/(Pa(1-Pa))|V) = pr*drho/[p1(1-p1)]
            nom = pr*(1-targ**2)/(p1*(1-p1))
            #            E(1/(Pa(1-Pa))|V) =  (1-pr)/[p0(1-p0)] + pr/[p1(1-p1)]
            denom = (1-pr)/(p0*(1-p0)) + pr/(p1*(1-p1))
            omega = nom/denom / (pr*p0*(1-p0))
        else:
            #            E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            nom = pr*p1/(1-p1)
            #            E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            denom = (1-pr)*p0/(1-p0) + pr*p1/(1-p1)
            omega = nom/denom / (pr*(1-p0))
        w = np.multiply(w.flatten(), omega).reshape(n, 1)
        m = tg.riskregmodel(y, a, x1, x2, x3, w, model)
        m.update(par)

    alpha0 = mle_coef[:x1.shape[1]].reshape(x1.shape[1], 1)
    def obj(alpha):
        return sum(sum(m.esteq(alpha, pr))**2)

    op = optimize.minimize(obj, alpha0, method='Nelder-Mead')
    alphahat = op['x']

    return {'estimate': alphahat, 'mle': mle_coef, 'prop': b1, 'op': op}
