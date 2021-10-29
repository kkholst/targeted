# -*- coding: utf-8 -*-
#
# Copyright (c) 2019-2020 Klaus K. Holst.  All rights reserved.

import ssm

class Kalman:
    """Documentation for kf

    """
    kalman = None
    def __init__(self, Y,T,Z,H,Q,a0,P0, K, L, x, c, d):
        """State space model

        Examples
        --------
        %TODO: write docstring
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
        LinearGaussian
            LinearGaussian object

        """
        self.kalman = ssm.CPP_Kalman(Y, T, Z, H, Q, a0, P0, K, L, x, c, d)

    def filter(self):
        self.kalman.filter()
        self.filter_estimates_ = self.kalman.filter_estimates
        self.llh_ = self.kalman.llh

    def smoother(self):
        self.kalman.smoother()
        self.smoother_estimates_ = self.kalman.smoother_estimates

    def update_model(self, Y, T, Z, H, Q, a0, P0, K, L, x, c, d):
        self.kalman.update_model(Y, T, Z, H, Q, a0, P0, K, L, x, c, d)

    def __repr__(self):
        return "Estimate: " + str(self.kalman)

    def __str__(self):
        return "Estimate: " + str(self.kalman)
