.. targeted documentation master file, created by
   sphinx-quickstart on Tue Sep 24 17:04:25 2019.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Targeted inference in python
===============================

``targeted`` provides a number of methods for semi-parametric
estimation.  The library also contains implementations of various
parametric models (including different discrete choice models) and
model diagnostics tools.

Relevant models includes binary regression models with binary exposure
and with nuisance models defined by additional covariates. Models for
the relative risk and risk differences where examined by
[Richardson_2018]_. Various missing data estimators and causal
inference models [Tsiatis_2006]_ also fits into this framework.

.. note:: This document is still very much work in progress.

.. toctree::
  :maxdepth: 1
  :caption: Quickstart & Intro

  Quickstart
  targeted

.. toctree::
  :maxdepth: 1
  :caption: Suggestions and bug reports

  Create a GitHub issue <https://github.com/kkholst/target/issues>


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

------

References
============
.. toctree::
   :maxdepth: 1
   :caption: References

.. [Tsiatis_2006] Tsiatis, A. (2006). Semiparametric theory and
  missing data. : Springer New York.
.. [Richardson_2018] Richardson, T. S., Robins, J. M., &
  Wang, L. (2017). On modeling and estimation for the relative risk
  and risk difference. Journal of the American Statistical
  Association, 112(519),
  1121–1130. http://dx.doi.org/10.1080/01621459.2016.1192546
