
|                     |                                                                                                      |                                                                                                                                     |                                                                                                            |                                                                                                              |
|:--------------------|:-----------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------|
| ``gof`` (R-package) | [<img src="https://travis-ci.org/kkholst/gof.svg?branch=master">](https://travis-ci.org/kkholst/gof) | [<img src="https://codecov.io/github/kkholst/gof/coverage.svg?branch=master">](https://codecov.io/github/kkholst/gof?branch=master) | [<img src="http://www.r-pkg.org/badges/version/gof">](http://cran.rstudio.com/web/packages/gof/index.html) | [<img src="http://cranlogs.r-pkg.org/badges/gof">](http://cranlogs.r-pkg.org/downloads/total/last-month/gof) |
|                     |                                                                                                      |                                                                                                                                     |                                                                                                            |                                                                                                              |

# Introduction

This library provides C++ classes for [targeted
inference](doc/targeted.md) and semi-parametric efficient estimators
as well as bindings for *python* and *R*. The library also contains
implementation of parametric models (including different
discrete choice models) and model diagnostics tools.

R
elevant models includes models binary regression models with binary
exposure and with nuisance models defined by additional
covariates. Models for the relative risk and risk differences where
examined by (Richardson et al 2018) and the odds ratio in
(Tchetgen-Tchetgen et al. 2010). Various missing data estimators and
causal inference models (Tsiatis 2006) also fits into this framework.

<p align="center">
  <img width="200"  src="doc/images/targeted.jpg">
</p>

## R
```r
library('target')

m <- lvm(exposure[-2] ~ 1*confounder,
         linpred.target[1] ~ 1,
         linpred.nuisance[-1] ~ 2*confounder)
distribution(m, ~exposure) <- binomial.lvm('logit')
m <- binomial.rr(m, 'response', 'exposure', 'linpred.target', 'linpred.nuisance')
dd <- sim(m,5e2,seed=1)
y <- dd$response; x <- dd$exposure; x1 <- x2 <- x3 <- cbind(1,dd$confounder); weights <- rep(1,length(y))
```

```r
summary(fit <- target::riskreg(y ~ a | 1 | x+z | 1, data=d, type="rr"))
```

## Python
```python
import target

inp = pkg_resources.resource_filename('target', '/data/d.csv')
d = pd.read_csv(inp, sep=',', header=0)
n = d.shape[0]
y, X2 = patsy.dmatrices('y ~ x+z', d)
a = d['a'].values.reshape(n, 1)
w = np.repeat(1, n).reshape(n, 1)
X1 = w
m = target.riskregmodel(y, a, X1, X2, X2, w, 'rr')

theta = [[1, 1, 1, 1]]
m.update(theta)
m.loglik()

```

## C++
```cpp
#include <target/target.h>

using namespace arma;
void main() {
	targeted::RR<double> model(y, a, x1, x2, x3, p, w);
	mat pp0 = model.TargetedBinary::pa();
    mat U = model.score(false);
    mat res = model.est(alpha);
}

```

### Installation

This program may be compiled as a shared library or as stand-alone python and R
libraries.


To compile and run the test program (depends on meson and ninja)
```
make
```	

Unit tests and code coverage
```
make test cov
```

Syntax check (requires *cppcheck* and *cclint*) 
```
make check cov
```

Code inspection (memory leaks, requires *valgrind*)
```
make valgrind
```

This documentation is based on Doxygen and can be compiled by
```
make doc
```

The R package can be built and installed with
```
make r
```

The python package can be built and installed with
```
make py
```

### Dependencies

To build the shared library ninja(s) are needed.
```
pip install ninja cclint 

```

The following are included as submodules:
1. Armadillo <http://arma.sourceforge.net/docs.html>
2. Catch2 (unit tests only) <https://github.com/catchorg/Catch2/blob/master/docs/Readme.md#top>
3. pybind11++ (python bindings only) <https://pybind11.readthedocs.io>
4. spdlog (logging) <https://github.com/gabime/spdlog>
```
git init
git submodules update
```

```
make py r
mkdir build; cd build; cmake .. -G Ninja && ninja
```
