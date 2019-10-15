
# Introduction

This library provides C++ classes for [targeted inference](targeted.md) 
and semi-parametric efficient estimators as
well as bindings for *R*. Relevant models includes models binary
regression models with binary exposure and with nuisance models
defined by additional covariates. Models for the relative risk and
risk differences where examined by Richardson et al \cite
richardson_rr_rd and the odds ratio in \cite tchetgen2010_or. Various
missing data estimators and causal inference models 
(see \cite tsiatis2006semiparametric) also fits into this framework.

![](images/targeted.jpg) width=150px 
<!--@image latex targeted.png-->

A double robust estimator for the average potential outcome is 
available in the class \c targeted::ACE (and in R: \c target::ace).
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




```{.py}
library(target)
library(h5)

m <- lvm(exposure[-2] ~ 1*confounder,
         linpred.target[1] ~ 1,
         linpred.nuisance[-1] ~ 2*confounder)
distribution(m,~exposure) <- binomial.lvm("logit")
m <- binomial.rr(m, "response","exposure","linpred.target","linpred.nuisance")
dd <- sim(m,5e2,seed=1)
y <- dd$response; x <- dd$exposure; x1 <- x2 <- x3 <- cbind(1,dd$confounder); weights <- rep(1,length(y))

```

```
#include <targeted.h>
using namespace arma;
void main() {
	targeted::RR<double> model(y, a, x1, x2, x2, p, w);
	mat pp0 = model.TargetedBinary::pa();
    mat U = model.score(false);
    mat res = model.est(alpha);
}

```




### Installation

This program may be built as a stand-alone program or as a
shared R-library.  Embedding into other languages (python, julia,
octave, stata), should be possible by linking to armadillo.

<!-- Program to be embedded in the [mets](http://lava.r-forge.r-project.org) package ... -->

To compile and run the test program
meson and 


```
make
```	

The R-test program may be run with
```
make rtest
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


### Dependencies

To build the shared library meson and ninja are needed.

  - Armadillo http://arma.sourceforge.net/docs.html
  - UnitTest++ (unit tests only) https://unittest-cpp.github.io/

Further the R interface depends, besides on the R development files,
the following packages
  - RcppArmadillo, Rcpp, DEoptim


Assuming a working python3 installation is available:
```
pip3 install meson ninja cclint --user

```
