import target
import pandas as pd
import numpy as np
import patsy
import pkg_resources
from scipy import optimize
import statsmodels.api as sm

inp = pkg_resources.resource_filename('target', '/data/d.csv')
d = pd.read_csv(inp, sep=',', header=0)
n = d.shape[0]
y, X2 = patsy.dmatrices('y ~ x+z', d)
a = d['a'].values.reshape(n, 1)
w = np.repeat(1, n).reshape(n, 1)
X1 = w
theta = [[1, 1, 1, 1]]
m = target.riskregmodel(y, a, X1, X2, X2, w, 'rr')

prop_mod = sm.GLM(endog=y, exog=X2, family=sm.families.Binomial())
prop = prop_mod.fit()
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
