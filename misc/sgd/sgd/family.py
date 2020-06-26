import numpy as np

class Family:
    def __init__(self):
        self.variance = 0.0

    def deviance(self, y: np.ndarray, mu: np.ndarray, wt: np.ndarray) -> float:
        pass


class Gaussian(Family):
    def __init__(self):
        self.variance = lambda x: 1.0

    def deviance(self, y, mu, wt): return np.sum(wt * ((y-mu)**2))

    def __str__(self):
        return "This is an instance of the Gaussian class."


class Poisson(Family):
    def __init__(self):
        self.variance = lambda x: x

    def deviance(self, y, mu, wt):
        r, flag = np.zeros(len(y)), y > 0.0
        r[~flag], r[flag] = mu[~flag] * wt[~flag], wt[flag] * ( (y[flag] * np.log(y[flag] / mu[flag]))  -  (y[flag] - mu[flag]) )
        return np.sum(2.0 * r)


class Gamma(Family):
    def __init__(self):
        self.variance = lambda x: x ** 2

    def deviance(self, y, mu, wt):
        r, flag  = np.zeros(len(y)), y != 0.0
        r[~flag], r[flag] = mu[~flag] * wt[~flag], wt[flag] * ( np.log(y[flag] / mu[flag]) - (y[flag] - mu[flag]) / mu[flag] )
        return np.sum(-2.0 * r)


class Binomial(Family):
    def __init__(self):
        self.variance = lambda x: x * (1.0 - x)

    def deviance(self, y: np.ndarray, mu: np.ndarray, wt: np.ndarray) -> float:
        r = np.zeros(len(y))
        r[y == 0], r[y == 1] = wt[y == 0] * np.log(1.0 - mu[y == 0]), wt[y == 1] * np.log(mu[y == 1])
        return np.sum(- 2.0 * r)

    ## Based on the C++ implementation; would be too slow in Python
    def slow_deviance(self, y: np.ndarray, mu: np.ndarray, wt: np.ndarray) -> float:
        return np.sum(np.array([ self.__deviance(y[i], mu[i], wt[i]) for i in np.arange(len(y)) ]))

    def __deviance(self, y: int, mu: float, wt: float) -> float:
        return 2.0 * wt * ( self.__xlogx(y, mu) + self.__xlogx(1-y, 1.0-mu) )

    def __xlogx(self, y: int, mu: float) -> float:
        return y * np.log(y / mu) if y != 0 else 0.0


class Inverse_Gaussian(Family):
    def __init__(self):
        pass

    def deviance(self, y, mu, wt):
        pass

class QuasiPoisson(Family):
    def __init__():
        pass

    def deviance(self, y, mu, wt):
        pass

class QuasiBinomial(Family):
    def __init__(self):
        pass

    def deviance(self, y, mu, wt):
        pass

class Quasi(Family):
    def __init__(self):
        pass

    def deviance(self, y, mu, wt):
        pass


if __name__ == "__main__":
     test_dat = np.genfromtxt('dev_check.csv', delimiter=',', skip_header=1)
     y = test_dat[:,0]
     mu = test_dat[:,1]

     tester = Binomial()
     print(tester.deviance(y, mu, np.ones(len(y))))




