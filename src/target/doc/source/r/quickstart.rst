
.. contents::



Quickstart
----------

.. code:: R

    m <- lvm(y ~ a+x, a~x)
    distribution(m,~ a+y) <- binomial.lvm()
    d <- sim(m,1e3,seed=1)

    head(d)

::

      y a          x
    1 0 1 -0.6264538
    2 1 0  0.1836433
    3 1 1 -0.8356286
    4 1 1  1.5952808
    5 0 1  0.3295078
    6 1 0 -0.8204684



.. code:: R

    library(targeted)

    a <- ace(y ~ a, nuisance=~x, data=d)
    summary(a)

::


    Augmented Inverse Probability Weighting estimator
      Response y (Outcome model: logistic regression):
    	 y ~ x
      Exposure a (Propensity model: logistic regression):
    	 a ~ x

                      Estimate Std.Err    2.5%   97.5%    P-value
     a=0               0.48506 0.02626  0.4336  0.5365  3.458e-76
     a=1               0.67794 0.02225  0.6343  0.7215 6.005e-204
    Outcome model:
     (Intercept)       0.44427 0.07306  0.3011  0.5875  1.196e-09
     x                 1.06929 0.08537  0.9020  1.2366  5.408e-36
    Propensity model:
     (Intercept)       0.06214 0.09258 -0.1193  0.2436  5.021e-01
     x                -0.92905 0.15311 -1.2291 -0.6289  1.297e-09

    Average Causal Effect (constrast: 'a=0' vs. 'a=1'):

       Estimate Std.Err    2.5%   97.5%   P-value
    RR   0.7155 0.04356  0.6301  0.8009 1.259e-60
    OR   0.4475 0.06268  0.3246  0.5703 9.383e-13
    RD  -0.1929 0.03295 -0.2575 -0.1283 4.791e-09

.. note::

    This document is work in progress

.. important::

    This document is work in progress.

.. tip::

    This document is work in progress.

.. warning::

    This document is work in progress.

.. figure:: testfig.png

    Caption goes here
