import kvaser
import netadp as na
import numpy as np
kvaser.reload("^netadp")


a = np.eye(3)
b = na.Test(a, a)
print(b.calc())

na.hello()