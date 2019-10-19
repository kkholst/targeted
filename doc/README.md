
# Introduction

This library provides C++ classes for [targeted inference](doc/targeted.md) 
and semi-parametric efficient estimators as
well as bindings for *python* and *R*. Relevant models includes models binary
regression models with binary exposure and with nuisance models
defined by additional covariates. Models for the relative risk and
risk differences where examined by Richardson et al \cite
richardson_rr_rd and the odds ratio in \cite tchetgen2010_or. Various
missing data estimators and causal inference models 
(see \cite tsiatis2006semiparametric) also fits into this framework.

![](doc/images/targeted.jpg)
<!--@image latex targeted.png-->

A double robust estimator for the average potential outcome is 
available in the class \c target::ACE.
Under *SUTVA* and *postivity* and *conditional exchangeability* 

\f[
\widehat{\alpha} = \frac{1}{n}\sum_{i=1}^n \Big\{\frac{I(A_i=a)Y_i}{\pi_a(X_i;
\gamma)} - \frac{I(A_i=a)-\pi_a(X_i; \gamma)}{\pi_a(X_i;
\gamma)}\mu(a,X_i; \beta)\Big\}
\f]

provides a consistent estimator of \f$E[Y^\ast(a)]\f$, provided either a 
consistent propensity model or outcome regression model. This follows
from rewriting the terms (assume for simplicity A binary) and consistency:

\f[
E\Big\{AY-\frac{\pi}{\pi}AY - \frac{AY}{\pi} - \frac{A-\pi}{\pi}\mu(1) 
\Big\} = E\left\{Y^\ast(1)\right\} + E\Big\{\left(\frac{A-\pi}{\pi}\right)
\left(Y^\ast(1)-\mu(1)\right)\Big\}
\f]

and by the law of iterated conditional expectation and the conditional
exchangeability assumption \f$A\perp Y^*\mid X\f$ consistency occurs when

\f[
E\Big\{E\left(\frac{A-\pi}{\pi}\mid X\right)
\left(E(AY-\mu(1)\mid X)\right)\Big\} = 0,
\f]

i.e. when either of the propensity model \f$\pi\f$ or outcome
regression \f$\mu\f$ are correctly specified.

 <!-- It follows by adding (Y-\pi Y/\pi) -->


The library also includes implementations of the risk-difference and
relative-risk  estimators with nuisance
model for the log odds-product (both *maximum likelihood* and *double-robust estimators*).
Here we specify model for the desired measure of association, i.e.
\f[
\theta(v) = \log \big(RR(v)\big)
\f]
\f[
\theta(v) = \mathrm{arctanh} \big(RD(v)\big)
\f]
and a model for the _odds-product_
\f[
\phi(v) = \log\left(\frac{p_{0}(v)p_{1}(v)}{(1-p_{0}(v))(1-p_{1}(v))}\right)
\f]
where  \f$p_a(V) = E(Y \mid A=a, V), a\in\{0,1\}\f$.
As described in \cite richardson_rr_rd this leads to variation
independent parameters \f$\alpha\f$ and \f$\beta\f$ circumenventing
computational and interpretational difficulties associated with
binomial regressions models with for example identify or log link
functions.



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

To build the shared library meson and ninja are needed.
```
pip install meson ninja cclint 

```

The following are included as submodules:
* Armadillo <http://arma.sourceforge.net/docs.html>
* UnitTest++ (unit tests only) <https://unittest-cpp.github.io/>
* pybind11++ (python bindings only) <https://pybind11.readthedocs.io>

