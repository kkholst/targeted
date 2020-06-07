from model import *
from family import *
from transfer import *

class glm(model):
    def __init__(self, lambda1: float, lambda2: float, family: str, transfer: str):
        super().__init__("Generalized Linear Model", lambda1, lambda2)
        self._family = family
        self._transfer = transfer
        try: self._family_instance = {
                    'gaussian': Gaussian(),
                    'poisson': Poisson(),
                    'gamma': Gamma(),
                    'binomial': Binomial()
                }[self._family]
        except: print("Warning: try one of the following families: gaussian, poisson, gamma, or binomial!!\n")
        try: self._transfer_instance = {
                    'identity': Identity(),
                    'exponential': Exponential(),
                    'inverse': Inverse(),
                    'logistic': Logistic()
                }[self._transfer]
        except: print("Warning: try one of the following transfer functions: identity, exponential, inverse, or logistic!!\n")


    def gradient(self, t: int, theta_old: np.ndarray, data: data_set) -> np.ndarray:
        datum = data.get_data_point(t)
        return ((datum._y - self._transfer_instance.h(datum._x @ theta_old)) * datum._x).T - self.gradient_penalty(theta_old)

    def scale_factor(self, ksi: float, at: float, datum: data_point, theta_old: np.ndarray, normx: float):
        return datum._y - self._transfer_instance.h((theta_old @ datum._x) - at * (self.gradient_penalty(theta_old) @ datum._x) + ksi * normx)

    def scale_factor_first_deriv(self, ksi: float, at: float, datum: data_point, theta_old: np.ndarray, normx: float):
        return self._transfer_instance.first_deriv((theta_old @ datum._x) - at * (self.gradient_penalty(theta_old) @ datum._x) + ksi * normx) * normx

    def scale_factor_second_deriv(self, ksi: float, at: float, datum: data_point, theta_old: np.ndarray, normx: float):
        return self._transfer_instance.second_deriv((theta_old @ datum._x) - at * (self.gradient_penalty(theta_old) @ datum._x) + ksi * normx) * (normx ** 2)


    def scale_factor_vec(self, ksi: float, a: np.ndarray, data: data_set, theta_old: np.ndarray, normx: np.ndarray):
        return data._Y - self._transfer_instance.h_vec((theta_old @ data._X) - a * (self.gradient_penalty(theta_old) @ data._X) + ksi * normx)

    def scale_factor_first_deriv_vec(self, ksi: float, a: np.ndarray, data: data_set, theta_old: np.ndarray, normx: np.ndarray):
        return self._transfer_instance.first_deriv_vec((theta_old @ data._X) - a * (self.gradient_penalty(theta_old) @ data._X) + ksi * normx) * normx

    def scale_factor_second_deriv_vec(self, ksi: float, a: np.ndarray, data: data_set, theta_old: np.ndarray, normx: np.ndarray):
        return self._transfer_instance.second_deriv_vec((theta_old @ data._X) - a * (self.gradient_penalty(theta_old) @ data._X) + ksi * normx) * (normx ** 2)


if __name__ == "__main__":
    data = data_set(np.array([[1.1,4.1], [2.1,3.6]]), np.array([0.25, 0.64]))

    m = glm(0.2, 0.3, "gaussian", "identity")
    # print(m._transfer_instance.h(4))
    # print(m.gradient(1, np.array([1.0,2.2]), data))
    # print(m.scale_factor(1.0, 0.5, data_point(np.array([1.0,2.2]), 1.4, 0), np.array([0.1,0.25]), 2.0))
    # print(m.scale_factor_second_deriv_vec(1.0, np.array([[0.5, 0.25], [1.0, 0.0]]), data, np.array([0.1,0.25]), np.array([2.0, 1.5])))



