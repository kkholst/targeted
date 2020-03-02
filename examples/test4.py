import target
import target.formula as tg
import pandas as pd
from pkg_resources import resource_filename

d = target.get_data()

val = tg.riskreg(d, 'y~a')
print(val['op'])

val = tg.riskreg(d, 'y~a', nuisance='x+z', propensity='x+z')
print(val['op'])

print('Interaction')
val = tg.riskreg(d, 'y~a', interaction='x', nuisance='x+z')
print(val['op'])

val = tg.riskreg(d, 'y~a', interaction='x', nuisance='x+z', propensity='x+z')
print(val['op'])


import target
import target.formula as tg
from target import iid, tcrossprod, robustse
from target import datatype as Input
import pandas as pd
from pkg_resources import resource_filename
d = target.get_data()
import patsy
import numpy as np
import numpy.linalg as npl
n = d.shape[0]
y, X2 = patsy.dmatrices('y ~ x+z', d)
a = d['a'].values.reshape(n, 1)
w = np.repeat(1, n).reshape(n, 1)
X1 = w
theta = np.matrix([[1, 1, 1, 1, 1, 1]]).transpose()

m = target.Riskreg(y, a, x1=X1, x2=X2, x3=X2, weights=w)

m = target.riskregmodel(y, a) #, x1=X1, x2=X2, x3=X2)
m.data(Input.w)[:5]

target.ace_est(y, a, X2, X2, theta, w, True)

import statsmodels.api as sm

prop_mod = sm.GLM(endog=a, exog=X2, family=sm.families.Binomial())
prop_mod.fit().summary()
b2 = prop_mod.fit().params
H2 = prop_mod.hessian(b2)
U2 = prop_mod.score_obs(b2)
prop_iid = np.matmul(npl.inv(H2), U2.transpose()).transpose()
vpr = np.matmul(prop_iid.transpose(), prop_iid)
vpr.diagonal()**.5

or_mod = sm.GLM(endog=y, exog=X2, family=sm.families.Gaussian())
or_mod.fit().summary()
b1 = or_mod.fit().params
H1 = or_mod.hessian(b1)
U1 = or_mod.score_obs(b1)
or_iid = np.matmul(npl.inv(H1), U1.transpose()).transpose()
vor = np.matmul(or_iid.transpose(), or_iid)
vor.diagonal()**.5

robustse(prop_mod)
robustse(or_mod)
