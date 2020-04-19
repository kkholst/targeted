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

.. code:: R

    summary(lm(Sepal.Length ~ Petal.Length, iris))

::


    Call:
    lm(formula = Sepal.Length ~ Petal.Length, data = iris)

    Residuals:
         Min       1Q   Median       3Q      Max
    -1.24675 -0.29657 -0.01515  0.27676  1.00269

    Coefficients:
                 Estimate Std. Error t value Pr(>|t|)
    (Intercept)   4.30660    0.07839   54.94   <2e-16 ***
    Petal.Length  0.40892    0.01889   21.65   <2e-16 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    Residual standard error: 0.4071 on 148 degrees of freedom
    Multiple R-squared:   0.76,	Adjusted R-squared:  0.7583
    F-statistic: 468.6 on 1 and 148 DF,  p-value: < 2.2e-16

.. figure:: testfig.png

    ... label:fig:testfig
