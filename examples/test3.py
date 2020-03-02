import target
import pandas as pd
import numpy as np
import patsy
import pkg_resources
from scipy import optimize
import statsmodels.api as sm

inp = pkg_resources.resource_filename('target', '/data/d.csv.gz')
d = pd.read_csv(inp, sep=',', header=0)
n = d.shape[0]
y, x2 = patsy.dmatrices('y ~ x+z', d)
a = patsy.dmatrix('-1+a', d)
x1 = x3 = w = patsy.dmatrix('1', d)

def mle_riskreg(y, a, x2, *args, **kwargs):
    one = np.matrix(np.repeat(1.0, len(y))).transpose()
    x1 = kwargs.get('x1', one)
    w = kwargs.get('weights', one)
    model = kwargs.get('model', 'rr')
    m = target.riskregmodel(y, a, x1, x2, one, w, model)

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


def estimate(y, a, optimal=True, *args, **kwargs):
    n = len(y)
    one = np.matrix(np.repeat(1.0, len(y))).transpose()
    x1 = kwargs.get('x1', one)
    x2 = kwargs.get('x2', one)
    x3 = kwargs.get('x3', one)
    w = kwargs.get('weights', one)
    model = kwargs.get('model', 'rr')

    prop_mod = sm.GLM(endog=a, exog=x3, family=sm.families.Binomial())
    b1 = prop_mod.fit().params
    mle = mle_riskreg(y, a, x2=x2, x1=x1, weights=w, model=model)
    mle_coef = mle['x']

    lp = np.matmul(x3, b1)
    pr = target.expit(lp)
    m = target.riskregmodel(y, a, x1, x2, x3, w, model)
    par = np.concatenate((mle_coef, b1))
    m.update(par)

    if optimal:
        pp = m.pr()
        p0 = pp[:, 1].flatten()
        p1 = pp[:, 2].flatten()
        targ = pp[:, 3].flatten()
        if (model == 'rd'):
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
        w = np.multiply(w.flatten(), omega).reshape(n,1)
        m = target.riskregmodel(y, a, x1, x2, x3, w, model)
        m.update(par)

    alpha0 = mle_coef[:x1.shape[1]]

    def obj(alpha):
        return sum(m.esteq(alpha, pr))**2

    op = optimize.minimize(obj, alpha0, method='Nelder-Mead')
    alphahat = op['x']

    return {'estimate': alphahat, 'mle': mle_coef, 'prop': b1, 'op': op}

val = estimate(y,a,x2=x2,x3=x2)

m = target.riskregmodel(y, a, x1, x2, x3, w, 'rr')
m.update([1,0,0,0])
m.pr()
    

m = target.riskregmodel(y, a, X1, X2, X2, w, 'rr')

prop_mod = sm.GLM(endog=y, exog=X2, family=sm.families.Binomial())
prop = prop_mod.fit().
print(prop.summary())
print(prop.params)
print(prop.cov_params())
prop_mod.hessian(prop.params)


m.update(theta)
res = m.loglik()
print(res)
res = m.hessian()
print(res)


def obj(theta):
    m.update(np.matrix(theta))
    return -m.loglik()


def jac(theta):
    m.update(np.matrix(theta))
    return -m.score().flatten()


op = optimize.minimize(obj, theta, method="BFGS", jac=jac)
cc = op['x']
print("MLE:", cc)

m.update(cc)
m.loglik()
res = m.hessian()
print(res)


def riskreg(data, target, model='rr', **kwargs):
    interaction = kwargs.get('interaction', '1')
    nuisance    = kwargs.get('nuisance',    '1')
    propensity  = kwargs.get('propensity',  '1')
    weights     = kwargs.get('weights',     '1')

    target = target + '-1'
    y, a = patsy.dmatrices(target, data)
    x1 = patsy.dmatrix(interaction, data)
    x2 = patsy.dmatrix(nuisance, data)
    x3 = patsy.dmatrix(propensity, data)
    w = patsy.dmatrix(weights, data)
    res = tg.riskreg(y, a, x1=x1, x2=x2, x3=x3, model=model)
    return res


################################################################################

import target as tg
import pandas as pd
import numpy as np
import patsy
from pkg_resources import resource_filename
d = pd.read_csv(resource_filename('target', '/data/d.csv'), sep=',', header=0)

riskreg(d, 'y~a', nuisance='x+z', propensity='x+z')

################################################################################

import target.formula as tg
import pandas as pd
from pkg_resources import resource_filename
d = pd.read_csv(resource_filename('target', '/data/d.csv'), sep=',', header=0)

tg.riskreg(d, 'y~a', nuisance='x+z', propensity='x+z')

