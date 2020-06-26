from sgd import *
from model import *


class ExplicitSGD(SGD):
    def __init__(self, n_samples: int, timer: time, **details):
        super().__init__(n_samples, timer, **details)

    def update(self, t: int, theta_old: np.ndarray, data: data_set, model: model, good_gradient: bool) -> np.ndarray:
        grad_t = model.gradient(t, theta_old, data)
        if not all(np.isfinite(grad_t)): good_gradient = False
        ## TODO add proper learn rate definition
        at = 1.0 / t
        return theta_old + (at * grad_t)

    def sync_members(self, theta_new: np.ndarray):
        super().sync_members(theta_new)
        return self


if __name__ == "__main__":
    print("hi")

    details = {"method": "implicit", "nparams": 5, "reltol": 1e-10, "npasses": 10,\
               "size": 10, "pass": True, "check": True, "truth": np.array([1.0, 1.0, 1.25, 0.5, 0.25]) }
    tester = SGD(100000, time, **details)


