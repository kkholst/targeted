# NOTE: Development has moved to https://github.com/kkholst/targeted 


[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Build
Status](https://github.com/kkholst/target/actions/workflows/target_check.yaml/badge.svg)](https://github.com/kkholst/target/actions/workflows/target_check.yaml)
[![codecov](https://codecov.io/gh/kkholst/target/branch/develop/graph/badge.svg)](https://codecov.io/gh/kkholst/target)

[![CRAN](https://www.r-pkg.org/badges/version-last-release/targeted)](https://CRAN.R-project.org/package=targeted)
[![PyPIversion](https://badge.fury.io/py/targeted.svg)](https://badge.fury.io/py/targeted)
[![RTD](https://readthedocs.org/projects/target/badge/?version=latest&style=flat)](https://target.readthedocs.io/en/latest/)


# Introduction

This library provides C++ classes for targeted inference
and semi-parametric efficient estimators as
well as bindings for python and R. The library also contains
implementation of parametric models (including different discrete
choice models) and model diagnostics tools.

Relevant models includes binary regression models with binary exposure
and with nuisance models defined by additional covariates. Models for
the relative risk and risk differences where examined by (Richardson
et al 2017). Various missing data estimators and causal inference models
(Bang & Robins 2005, Tsiatis 2006) also fits into this framework.

# Documentation

The main documentation can be found here:
https://targetlib.org/ ([PDF version](https://target.readthedocs.io/_/downloads/en/latest/pdf/))


## Examples

### R

https://kkholst.github.io/targeted

```r
library('targeted')

# Simulation
m <- lvm(y[-2] ~ 1*x,
         lp.target[1] ~ 1,
         lp.nuisance[-1] ~ 2*x)
distribution(m, ~a) <- binomial.lvm('logit')
m <- binomial.rr(m, 'y', 'a', 'lp.target', 'lp.nuisance')
dd <- sim(m, 5e2, seed=1)

# Double-robust estimator
summary(fit <- targeted::riskreg(y ~ a | 1 | x | x, data=dd, type="rr"))
#
#  Relative risk model
#   Response:  y
#   Exposure:  a
#
#             Estimate Std.Err    2.5%    97.5%   P-value
# log(RR):
#  (Intercept)  0.86136 0.11574  0.6345  1.08820 9.895e-14
# log(OP):
#  (Intercept) -0.88518 0.22802 -1.3321 -0.43827 1.036e-04
#  x            2.35193 0.28399  1.7953  2.90854 1.213e-16
# logit(Pr):
#  (Intercept) -0.07873 0.08857 -0.2523  0.09485 3.740e-01
#  x            0.02894 0.08291 -0.1336  0.19145 7.270e-01
```

### Python

https://pypi.org/project/targeted/

```python
import targeted as tg
from targeted.formula import riskreg

d = tg.getdata()
val = riskreg(d, 'y~a', interaction='x', nuisance='x+z')

print(val)
## Riskreg. Estimate: [ 1.17486406 -0.23623467]
```

### C++

https://www.targetlib.org/cppapi/

```cpp
#include <target/target.h>
using namespace arma;

void main() {

  ...

  targeted::RR<double> model(y, a, x1, x2, x3, p, w);
  mat pp0 = model.TargetedBinary::pa();
  mat U = model.score(false);
  mat res = model.est(alpha);
}

```

## Installation

This program may be compiled as a shared library or as stand-alone
python and R libraries.

To compile and run the unit tests:
```
make test
```

Syntax checks (requires ``cppcheck`` and ``cclint``), code coverage,
and check for memory leaks
```
make check coverage
make valgrind
```

### R

The R package can be built and installed with
```
make r
```

### Python

The python package can be installed directly from PyPi (wheels
available for Mac OS X and Linux):
```
pip install targeted
```
or installed from source
```
make py
```

### Dependencies

The following dependencies are included as submodules:

1. Armadillo <http://arma.sourceforge.net/docs.html>
2. doctest (unit tests only) <https://github.com/onqtam/doctest/>
3. pybind11++ (python bindings only) <https://pybind11.readthedocs.io>
4. spdlog (logging) <https://github.com/gabime/spdlog>
