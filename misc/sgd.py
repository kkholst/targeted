import numpy as np
from scipy.optimize import brentq
from functools import partial
from scipy.stats import invgamma
import statsmodels.api as sm
import pandas as pd
import os


class SGD:
    def __init__(self):
        pass

    def convergence(self, theta_new: np.ndarray, theta_old: np.ndarray) -> bool:
        pass

    def __setattr__(self, theta_new: np.ndarray):
        pass

    def early_stop(self):
        pass


class Implicit_SGD(SGD):
    def __init__(self, X, Y, guess, transfer_func = lambda x: x, alpha = 1.0):
        self._N = len(X)
        self._X = np.array(X)
        self._Y = np.array(Y)
        self._a = np.array(alpha / np.arange(1, self._N + 1))
        self._theta = np.array(guess)
        self._alpha = alpha
        self._transfer_func = transfer_func

    def optimize(self):
        for n, (x, y) in enumerate(zip(self._X, self._Y)):
            r = self._a[n] * (y - self._transfer_func(self._theta @ x))
            lb, ub = 0, r
            if r < 0: lb, ub = ub, lb
            ksi = brentq(partial(self._glm_im, n, x, y), lb, ub, maxiter = 100)
            self._theta += ksi * x

    def _glm_im(self, n, x, y, w): return w - self._a[n] * ( y - self._transfer_func( (self._theta @ x) + (np.linalg.norm(x) ** 2) * w ) )


    @staticmethod
    def example(func, bounds = (-1, 1)):
        return brentq(func, bounds[0], bounds[1])


if __name__ == "__main__":
    print("hi")

    # data = sm.datasets.scotland.load(as_pandas=True)
    # data.exog = sm.add_constant(data.exog, prepend=False)
    # data.exog.to_csv(os.path.join(os.getcwd(), 'glm_test.csv'), index=False, header = True)

    data = sm.datasets.scotland.load()
    data.exog = sm.add_constant(data.exog, prepend=False)
    data.exog["Response"] = data.endog
    # print(data.exog)


    X = data.exog
    Y = data.endog

    tester = Implicit_SGD(X, Y, np.ones(X.shape[1]), alpha = 1.0)
    print(tester._theta)
    tester.optimize()
    print(tester._theta)




