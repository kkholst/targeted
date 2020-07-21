from .sgd import *
from .model import *
from functools import partial


# Root finding function
# Evaluate the following expression, as well as its first and second order derivatives
# \ksi - \ell(x^T \theta + ||x||^2 * \ksi)
class ImplicitFn:
    def __init__(self, model: model, a: float, d: data_point, t: np.ndarray, n: float):
        self.__model = model
        self.__at = a
        self.__datum = d
        self.__theta_old = t
        self.__normx = n

    def __call__(self, ksi: float) -> tuple:
        value = ksi - self.__at * self.__model.scale_factor(ksi, self.__at, self.__datum, self.__theta_old, self.__normx)
        # first = 1 + self.__at * self.__model.scale_factor_first_deriv(ksi, self.__at, self.__datum, self.__theta_old, self.__normx)
        # second = self.__at * self.__model.scale_factor_second_deriv(ksi, self.__at, self.__datum, self.__theta_old, self.__normx)
        return value # , first, second



class ImplicitSGD(SGD):
    def __init__(self, n_samples: int, timer: time, **details):
        super().__init__(n_samples, timer, **details)
        # self.__delta = details["delta"]

    def update(self, t: int, theta_old: np.ndarray, data: data_set, model: model, good_gradient: bool) -> np.ndarray:
        ## TODO add proper learn rate definition
        at = 1.0 / t

        datum = data.get_data_point(t)
        normx = np.linalg.norm(datum._x)

        ksi = 0
        r, lower, upper = at * model.scale_factor(ksi, at, datum, theta_old, normx), 0, 0
        if r < 0: lower = r
        else: upper = r

        if lower != upper:
            implicit_fn = ImplicitFn(model, at, datum, theta_old, normx)
            ksi = brentq(implicit_fn, lower, upper, maxiter = 100)
        else: ksi = lower

        return theta_old + ksi * datum._x - at * model.gradient_penalty(theta_old)


