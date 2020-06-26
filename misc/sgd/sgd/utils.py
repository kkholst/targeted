import numpy as np
import math

sign = np.vectorize(lambda x: math.copysign(1, x))


class DataLoader:
    def __init__(self, dbcon = None, data_path = None, data_uri = None):
        self._dbcon = dbcon,
        self._data_path = data_path
        self_data_uri = data_uri

    def load_from_file(self, path: str):
        pass

    def load_from_query(self, query_str: str):
        pass

    def load_from_uri(self, uri: str):
        pass


class data_point:
    def __init__(self, x: np.ndarray, y: float, i: int):
        self._x, self._y, self._i = x, y, i


class data_set:
    def __init__(self, X: np.ndarray, Y: np.ndarray):
        self._X, self._Y, self._n, self._p = X, Y, X.shape[0], X.shape[1]

    ## Replace this with a dunder call
    def get_data_point(self, t: int) -> data_point:
        t = (t-1) % self._n
        return data_point(self._X[t], self._Y[t], t)


