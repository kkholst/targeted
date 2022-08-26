#!/usr/bin/env python3

import targeted_template as tg
import numpy as np

x = np.eye(3)

tg.add(x, x)

tg.expit(x)

tg.add1([1,2,3])

a = np.array(x.copy(), dtype='float64', order='F')
tg.scale2(a)
tg.scale2(a)
a
