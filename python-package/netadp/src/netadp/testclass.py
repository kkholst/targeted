# -*- coding: utf-8 -*-
#
# Copyright (c) 2019-2022 Klaus K. Holst.  All rights reserved.

import netadp as tg

class Test:
    """Documentation for testclass

    """

    def __init__(self, x, y, **kwargs):
        """Test class

        Examples
        --------

        Parameters
        ----------
        x:  numpy.array, optional
            Design matrix
        y:  numpy.array, optional
            Design matrix
        type: str
            Default: 'default', Advanced: 'advanced'

        Returns
        -------
        Test
            Test object

        """

        self.type = kwargs.get('type', 'default')
        self.model = tg.testclass(x, y, self.type)

    def calc(self):
        return self.model.calc()

    def __repr__(self):
        return "type: " + str(self.type)

    def __str__(self):
        return "type: " + str(self.type)
