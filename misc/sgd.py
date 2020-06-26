import numpy as np
from scipy.optimize import brentq
from functools import partial
from scipy.stats import invgamma
import statsmodels.api as sm
import pandas as pd
import os
import time


class SGD:
    def __init__(self, n_samples: int, timer: time, **details):
        self._name = details["method"]
        self._n_params = details["nparams"]
        self._reltol = details["reltol"]    # relative tolerance for convergence
        self._n_passes = details["npasses"] # number of passes over data
        self._size = details["size"]        # number of estimates to be recorded (log-uniformly)
        self._estimates = np.zeros((self._n_params, self._size))
        self._last_estimate = np.zeros(self._n_params)
        self._times = np.zeros(self._size)
        self._timer = timer
        self._t = 0
        self._n_recorded = 0              # number of coefs that have been recorded
        self._pos = np.zeros(self._size)  # the iteration of recorded coefs
        self._pass = details["pass"]      # force running for n_passes on data
        self._check = details["check"]

        if self._check: self._truth = details["truth"]

        ## Select the iterations to store estimates
        n_iters = n_samples * self._n_passes
        self._pos = (10.0 ** (np.arange(self._size) * np.log10(float(n_iters)) / (self._size-1))).astype(int)
        if self._pos[-1] != n_iters: self._pos[-1] = n_iters

        ## TODO: Set learning rate
        ## ...


    def get_value_of(self, attribute: str):
        try: return self.__dict__["_" + attribute]
        except KeyError as e: print(attribute + " is not an attribute of the caller.")

    def convergence(self, theta_new: np.ndarray, theta_old: np.ndarray) -> bool:
        if self._check:
            qe = np.mean(np.mean((theta_new - self._truth) ** 2))
            print(qe)
            if qe < 0.001: return True
        elif not self._pass:
            qe = np.mean(np.mean(np.abs(theta_new - theta_old))) / np.mean(np.mean(np.abs(theta_old)))
            if qe < self._reltol: return True
        return False

    def sync_members(self, theta_new: np.ndarray):
        self._last_estimate = theta_new
        self._t += 1
        if self._t == self._pos[self._n_recorded]:
            self._estimates[:, self._n_recorded] = theta_new
            ## TODO record elapsed time
            self._n_recorded += 1
            while (self._n_recorded < self._size) and (self._pos[self._n_recorded-1] == self._pos[self._n_recorded]):
                self._estimates[:, self._n_recorded] = theta_new
                ## TODO record elapsed time
                self._n_recorded += 1

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

    details = {"method": "implicit", "nparams": 5, "reltol": 1e-10, "npasses": 10,\
               "size": 10, "pass": True, "check": True, "truth": np.array([1.0, 1.0, 1.25, 0.5, 0.25]) }
    tester = SGD(100000, time, **details)


    # data = sm.datasets.scotland.load(as_pandas=True)
    # data.exog = sm.add_constant(data.exog, prepend=False)
    # data.exog.to_csv(os.path.join(os.getcwd(), 'glm_test.csv'), index=False, header = True)

    # data = sm.datasets.scotland.load()
    # data.exog = sm.add_constant(data.exog, prepend=False)
    # data.exog["Response"] = data.endog
    # print(data.exog)


    # X = data.exog
    # Y = data.endog

    # tester = Implicit_SGD(X, Y, np.ones(X.shape[1]), alpha = 1.0)
    # print(tester._theta)
    # tester.optimize()
    # print(tester._theta)




