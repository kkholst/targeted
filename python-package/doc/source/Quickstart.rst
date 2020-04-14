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

.. code:: python

    import target as tg

    d = tg.getdata() # returns a Pandas DataFrame
    print(d.head())

.. code:: none

         y  a         x         z
    0    0  0 -0.626454  1.134965
    1    0  0  0.183643  1.111932
    2    0  0 -0.835629 -0.870778
    3    1  0  1.595281  0.210732
    4    1  1  0.329508  0.069396

Her ``y`` is the binary response variable, ``a`` binary the exposure,
and ``x``, ``z`` covariates.

Next we estimate the *risk difference* of the exposed vs the non-exposed
.. code:: python

   import numpy as np
   from patsy import dmatrices

   y, X = dmatrices('y ~ x+z', d)
   a = d['a']
   ones = np.ones((y.size,1))
   tg.riskreg(y=y, a=a, x1=ones, x2=X)

.. code:: none

   Riskreg. Estimate: [1.02819001]

Or using the formula syntax:

.. code:: python

   from targeted.formula import riskreg

   riskreg(d, 'y~a')

.. code:: none

   Riskreg. Estimate: [1.02819001]
