
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![travis](https://travis-ci.org/kkholst/targeted.svg?branch=master)](https://travis-ci.org/kkholst/targeted)
[![coverage](https://codecov.io/github/kkholst/targeted/coverage.svg?branch=master)](https://codecov.io/github/kkholst/targeted?branch=master)
[![license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![cran](https://www.r-pkg.org/badges/version-last-release/targeted)](http://cranlogs.r-pkg.org/downloads/total/last-month/targeted)
<!-- badges: end -->

# Targeted Inference in R: targeted <a href='https://target.readthedocs.io/en/latest/r/index.html'><img src='man/figures/logo.svg' align="right" height="75" /></a>

## Installation

You can install the released version of targeted from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("targeted")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
remotes::install_github("kkholst/targeted")
```

## Examples

### Targeted risk regression

``` r
library(targeted)
library(magrittr)
```

Simulate some data:

``` r
m <- lvm() %>%
    regression(a ~ x+z) %>%
    regression(lp.target ~ 1) %>%
    regression(lp.nuisance ~ x + z) %>%
    distribution('a', binomial.lvm("logit")) %>%
    binomial.rr('y', 'a', 'lp.target', 'lp.nuisance')

par <- c('a'=-2, 'lp.target'=1, 'lp.nuisance'=-1, 'lp.nuisance~x'=2)
d <- lava::sim(m, n=1e4, seed=1, p=par) %>%
    subset(select=c('y', 'a','x','z'))

head(d)
#>   y a          x          z
#> 1 0 0 -0.6264538 -0.8043316
#> 2 0 0  0.1836433 -1.0565257
#> 3 0 0 -0.8356286 -1.0353958
#> 4 0 0  1.5952808 -1.1855604
#> 5 1 0  0.3295078 -0.5004395
#> 6 0 0 -0.8204684 -0.5249887
```

``` r
fit <- riskreg(y ~ a, nuisance=~x+z, data=d, type="rr")
fit
#>             Estimate Std.Err   2.5% 97.5%    P-value
#> (Intercept)   0.9722 0.02896 0.9155 1.029 4.281e-247
```

Here the same design matrix is used for both the propensity model and
the nuisance parameter (odds-product) model

``` r
summary(fit)
#> riskreg(formula = y ~ a, nuisance = ~x + z, data = d, type = "rr")
#> 
#> Relative risk model
#>   Response:  y 
#>   Exposure:  a 
#> 
#>              Estimate Std.Err    2.5%   97.5%    P-value
#> log(RR):                                                
#>  (Intercept)   0.9722 0.02896  0.9155  1.0290 4.281e-247
#> log(OP):                                                
#>  (Intercept)  -0.9636 0.06603 -1.0930 -0.8342  3.072e-48
#>  x             2.0549 0.07901  1.9000  2.2098 4.182e-149
#>  z             1.0329 0.06728  0.9010  1.1648  3.468e-53
#> logit(Pr):                                              
#>  (Intercept)  -1.9753 0.05631 -2.0856 -1.8649 1.284e-269
#>  x             0.9484 0.04186  0.8664  1.0305 1.235e-113
#>  z             1.0336 0.04878  0.9380  1.1292  1.187e-99
```

Double-robustness illustrated by using a wrong propensity model but a
correct nuisance paramter (odds-product) model:

``` r
riskreg(y ~ a, nuisance=~x+z, propensity=~z, data=d, type="rr")
#>             Estimate Std.Err   2.5% 97.5%    P-value
#> (Intercept)   0.9709 0.02893 0.9142 1.028 7.053e-247
```

Or vice-versa

``` r
riskreg(y ~ a, nuisance=~z, propensity=~x+z, data=d, type="rr")
#>             Estimate Std.Err   2.5% 97.5%    P-value
#> (Intercept)   0.9931 0.03597 0.9226 1.064 8.286e-168
```

Whereas the MLE yields a biased estimate of the relative risk:

``` r
fit_mle <- with(d, riskreg_mle(y, a, x1=model.matrix(~1,d), x2=model.matrix(~z, d)))
estimate(fit_mle, 1)
#>      Estimate Std.Err  2.5% 97.5% P-value
#> [p1]    1.289 0.02855 1.233 1.345       0
#> 
#>  Null Hypothesis: 
#>   [p1] = 0
```

To obtain an estimate of the risk-difference (here wrong model) we
simply chance the `type` argument

``` r
riskreg(y ~ a, nuisance=~x+z, data=d, type="rd")
#>             Estimate Std.Err   2.5%  97.5%    P-value
#> (Intercept)   0.5102 0.01613 0.4786 0.5418 1.135e-219
```

Interactions with the exposure can be examined with the `target`
argument

``` r
riskreg(y ~ a, target=a~x, nuisance=~x+z, data=d, type="rr")
#>             Estimate Std.Err    2.5%    97.5%    P-value
#> (Intercept)   1.0241 0.03659  0.9524  1.09584 1.986e-172
#> x            -0.0825 0.03469 -0.1505 -0.01451  1.739e-02
```
