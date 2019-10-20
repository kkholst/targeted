import target
import pandas as pd
import numpy as np
import patsy
import pkg_resources
from scipy import optimize
import statsmodels.api as sm
import target.formula as tg

inp = pkg_resources.resource_filename('target', '/data/d.csv')
d = pd.read_csv(inp, sep=',', header=0)
n = d.shape[0]
y, X2 = patsy.dmatrices('y ~ x+z', d)
a = d['a'].values.reshape(n, 1)
w = np.repeat(1, n).reshape(n, 1)
X1 = w
theta = [[1, 1, 1, 1]]
m = target.riskregmodel(y, a, X1, X2, X2, w, 'rr')
# m = target.riskregmodel(y, a, X2, X2, X2, w, 'rr')
# par = np.matrix([[1,2,3]]).transpose()
# m.esteq(par, w)

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

val = target.riskreg(y, a, x2=X2)
print(val['op'])

val = target.riskreg(y, a, x2=X2, x3=X2)
print(val['op'])

val = target.riskreg(y, a, x2=X1, x3=X2)
print(val['op'])

print('risk difference')
val = target.riskreg(y, a, x2=X2, x3=X2, model='rd')
print(val['op'])


print('-----------------')
print('Formula syntax')
val = tg.riskreg(d, 'y~a')
print(val['op'])
val = tg.riskreg(d, 'y~a', nuisance='x+z', propensity='x+z')
print(val['op'])
print('Interaction')
val = tg.riskreg(d, 'y~a', interaction='x', nuisance='x+z')
print(val['op'])
