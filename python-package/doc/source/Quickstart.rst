.. _code_directive:

Quickstart
====================


Installation
--------------------

Install with ``pip``:

.. code-block:: console

    pip install targeted

.. note:: Presently binary wheels are available for Linux
	  systems and Mac OS X running Python>=3.6. Windows
	  installations must be built from source.


Risk regression
--------------------

As an illustration data is loaded from the package

.. ipython:: python

   import targeted as tg

   d = tg.getdata() # returns a Pandas DataFrame
   print(d.head())

Here ``y`` is the binary response variable, ``a`` binary the exposure,
and ``x``, ``z`` covariates.

Next we estimate the *risk difference* of the exposed vs the non-exposed

.. ipython:: python

   import numpy as np
   from patsy import dmatrices

   y, X = dmatrices('y ~ x+z', d)
   a = d['a']
   ones = np.ones((y.size,1))
   tg.riskreg(y=y, a=a, x1=ones, x2=X)

Or using the formula syntax:

.. ipython:: python

   from targeted.formula import riskreg

   riskreg(d, 'y~a')


R package
--------------------

.. code:: r

   print(2+2)
