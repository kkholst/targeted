import targeted as tg
import numpy as np
from patsy import dmatrices
import statsmodels.api as sm

d = tg.getdata() # returns a Pandas DataFrame
y, X = dmatrices('y ~ x+z', d)
a = d['a']
ones = np.ones((y.size,1))
tg.riskreg(y=y, a=a, x1=ones, x2=X, type='rr')
w = ones.squeeze()

m = sm.GLM(endog=a, exog=X, freq_weights=w, family=sm.families.Binomial())
m.fit().summary()
