import numpy as np

class Transfer:
    def __init__(self, h):
        self.h = h
        self.g = 0.0
        self.first_deriv = 0.0
        self.second_deriv = 0.0

    def h_vec(self, u: np.ndarray) -> np.ndarray:
        return np.vectorize(self.h)(u)

    def g_vec(self, u: np.ndarray) -> np.ndarray:
        return np.vectorize(self.g)(u)

    def first_deriv_vec(self, u: np.ndarray) -> np.ndarray:
        return np.vectorize(self.first_deriv)(u)

    def second_deriv_vec(self, u: np.ndarray) -> np.ndarray:
        return np.vectorize(self.second_deriv)(u)


class Identity(Transfer):
    def __init__(self):
        super().__init__(lambda x: x)
        self.g = self.h
        self.first_deriv = lambda x: 1.0
        self.second_deriv = lambda x: 0.0

    def __str__(self):
        return "This is an instance of the Identity class."


class Inverse(Transfer):
    def __init__(self):
        super().__init__(lambda x: -1.0 / x)
        self.g = self.h
        self.first_deriv = lambda x: 1.0 / (x**2) if self.valid(x) else 0.0
        self.second_deriv = lambda x: -2.0 / (x**3) if self.valid(x) else 0.0
        self.valid = lambda x: True if x != 0.0 else False


class Exponential(Transfer):
    def __init__(self):
        super().__init__(lambda x: np.exp(x))
        self.g = lambda x: np.log(x) if x > 0.0 else 0.0
        self.first_deriv = self.h
        self.second_deriv = self.h

class Logistic(Transfer):
    def __init__(self):
        super().__init__(lambda x: self.__sigmoid(x))
        self.g = lambda x: np.log(x / (1.0 - x)) if all([x > 0.0, x < 1.0]) else 0.0
        self.first_deriv = lambda x: self.__sigmoid(x) * (1.0 - self.__sigmoid(x))
        self.second_deriv = lambda x: 2 * (self.__sigmoid(x) ** 3) - 3 * (self.__sigmoid(x) ** 2) + 2 * self.__sigmoid(x)

    def __sigmoid(self, x): return 1.0 / (1.0 + np.exp(-x))


# if __name__ == "__main__":
    # tester = Transfer(lambda x: x ** 2)
    # print(all(tester.h_vec(np.array([4])) == 16))
    # print(all(tester.h_vec(np.array([1,2,4,5])) == [1,4,16,25]))

    # tester = Identity()
    # print((tester.h(2) == 2) and (tester.g(2) == 2))
    # print(all(tester.h_vec(np.array([4])) == 4))
    # print(all(tester.h_vec(np.array([1,2,4,5])) == [1,2,4,5]))
    # print((tester.first_deriv(10.0) == 1) and (tester.second_deriv(101) == 0.0))

    # tester = Inverse()
    # print((tester.h(2) == -1.0 / 2) and (tester.g(2) == -1.0 / 2))
    # print(all(tester.h_vec(np.array([4])) == -1.0 / 4))
    # print(all(tester.h_vec(np.array([1,2,4,5])) == [-1.0 / 1, -1.0 / 2, -1.0 / 4, -1.0 / 5]))
    # print(all([tester.first_deriv(10.0) == 1.0/ 10.0**2, tester.second_deriv(101) == -2.0 / 101**3, tester.first_deriv(0) == 0.0]))

    # tester = Exponential()
    # print(all([tester.h_vec(10) == np.exp(10), tester.g(10) == np.log(10)]))
    # print(all(tester.h_vec(np.array([10])) == np.exp(10)))
    # print(tester.h_vec(10) == np.exp(10))
    # print(all(tester.h_vec(np.array([1,2,3,4])) == np.exp(np.array([1,2,3,4]))))

    # tester = Logistic()
    # print(all(tester.h_vec(np.array([0.01, 1.0, 10.0 , 100.0])) == 1.0 / (1.0 + np.exp(-np.array([0.01, 1.0, 10.0, 100.0])))))
    # print(all([tester.g(-1) == 0.0, tester.g(1) == 0.0, tester.g(0.25) == np.log(0.25 / 0.75)]))
    # print(tester.second_deriv_vec(np.array([0.75, 0.25])))



