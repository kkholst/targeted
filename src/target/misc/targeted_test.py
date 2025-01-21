import targeted as tg
import numpy as np
import patsy
import scipy.optimize as opt
import statsmodels.api as sm
import targeted.formula as tgf
import logging
try:
    import coloredlogs
    coloredlogs.install(level='INFO')
except ModuleNotFoundError:
    logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

######################################################################

d = tg.getdata()
n = d.shape[0]
y, X2 = patsy.dmatrices('y ~ x+z', d)
a = d['a'].values.reshape(n, 1)
w = np.repeat(1, n).reshape(n, 1)
X1 = w
theta = [[1, 1, 1, 1]]
m = tg.riskregmodel(y, a, X1, X2, X2, w, 'rr')
# m = tg.riskregmodel(y, a, X2, X2, X2, w, 'rr')
# par = np.matrix([[1,2,3]]).transpose()
# m.esteq(par, w)

prop_mod = sm.GLM(endog=y, exog=X2, family=sm.families.Binomial())
prop = prop_mod.fit()
logger.info("\n%s", prop.summary())
logger.info("Parameters\n%s", prop.params)
logger.info("Asym.variance\n%s", prop.cov_params())
prop_mod.hessian(prop.params)

m.update(theta)
res = m.loglik()
logger.info("loglik\n%s", res)
res = m.hessian()
logger.info("hessian\n%s", res)


def obj(theta):
    m.update(np.matrix(theta))
    return -m.loglik()


def jac(theta):
    m.update(np.matrix(theta))
    return -m.score().flatten()


op = opt.minimize(obj, np.array(theta).flatten(), method="BFGS", jac=jac)
cc = op['x']
logger.info("MLE:\n\t%s", cc)

m.update(cc)
m.loglik()
res = m.hessian()
logger.info("Hessian:\n%s", res)


######################################################################

logger.info('-----------------')
val = tg.riskreg(y, a, x2=X2)
logger.info(val)

val = tg.riskreg(y, a, x2=X2, x3=X2)
logger.info(val)

val = tg.riskreg(y, a, x2=X1, x3=X2)
logger.info(val)

val = tg.riskreg(y, a, x2=X2, x3=X2, model='rd')
logger.info(val)

logger.info('-----------------')
logger.info('Formula syntax')
val = tgf.riskreg(d, 'y~a')
logger.info(val)

val = tgf.riskreg(d, 'y~a', nuisance='x+z', propensity='x+z')
logger.info(val)

logger.info('Interaction')
val = tgf.riskreg(d, 'y~a', interaction='x', nuisance='x+z')
logger.info(val)
