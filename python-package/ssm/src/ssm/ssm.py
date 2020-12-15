# -*- coding: utf-8 -*-
#
# Copyright (c) 2019-2020 Klaus K. Holst.  All rights reserved.

import ssm
import numpy as np

class kf:
    """Documentation for kf

    """
    obj = None

    def __init__(self, h, q, y):
        """State space model

        Examples
        --------

        Parameters
        ----------
        h: numpy.array
            observation error variance
        q: numpy.array
            state error variance
        y: numpy.array
            Observed time series

        Returns
        -------
        kf
            kf object

        """
        obj = ssm.kalmanfilter(h,q,y)


    def __repr__(self):
        return "Estimate: " + str(self.obj)

    def __str__(self):
        return "Estimate: " + str(self.obj)
