# Risk regression

Risk regression with binary exposure and nuisance model for the
odds-product.

Let \\A\\ be the binary exposure, \\V\\ the set of covariates, and \\Y\\
the binary response variable, and define \\p_a(v) = P(Y=1 \mid A=a,
V=v), a\in\\0,1\\\\.

The **target parameter** is either the *relative risk*
\$\$\mathrm{RR}(v) = \frac{p_1(v)}{p_0(v)}\$\$ or the *risk difference*
\$\$\mathrm{RD}(v) = p_1(v)-p_0(v)\$\$

We assume a target parameter model given by either \$\$\log\\RR(v)\\ =
\alpha^t v\$\$ or \$\$\mathrm{arctanh}\\RD(v)\\ = \alpha^t v\$\$ and
similarly a working linear **nuisance model** for the *odds-product*
\$\$\phi(v) =
\log\left(\frac{p\_{0}(v)p\_{1}(v)}{(1-p\_{0}(v))(1-p\_{1}(v))}\right) =
\beta^t v\$\$.

A **propensity model** for \\E(A=1\|V)\\ is also fitted using a logistic
regression working model \$\$\mathrm{logit}\\E(A=1\mid V=v)\\ = \gamma^t
v.\$\$

If both the odds-product model and the propensity model are correct the
estimator is efficient. Further, the estimator is consistent in the
union model, i.e., the estimator is double-robust in the sense that only
one of the two models needs to be correctly specified to get a
consistent estimate.

## Usage

``` r
riskreg(
  formula,
  nuisance = ~1,
  propensity = ~1,
  target = ~1,
  data,
  weights,
  type = "rr",
  optimal = TRUE,
  std.err = TRUE,
  start = NULL,
  mle = FALSE,
  ...
)
```

## Arguments

- formula:

  formula (see details below)

- nuisance:

  nuisance model (formula)

- propensity:

  propensity model (formula)

- target:

  (optional) target model (formula)

- data:

  data.frame

- weights:

  optional weights

- type:

  type of association measure (rd og rr)

- optimal:

  If TRUE optimal weights are calculated

- std.err:

  If TRUE standard errors are calculated

- start:

  optional starting values

- mle:

  Semi-parametric (double-robust) estimate or MLE (TRUE gives MLE)

- ...:

  additional arguments to unconstrained optimization routine (nlminb)

## Value

An object of class '`riskreg.targeted`' is returned. See
[`targeted-class`](targeted-class.md) for more details about this class
and its generic functions.

## Details

The 'formula' argument should be given as
`response ~ exposure | target-formula | nuisance-formula` or
`response ~ exposure | target | nuisance | propensity`

E.g., `riskreg(y ~ a | 1 | x+z | x+z, data=...)`

Alternatively, the model can specifed using the target, nuisance and
propensity arguments: `riskreg(y ~ a, target=~1, nuisance=~x+z, ...)`

The `riskreg_fit` function can be used with matrix inputs rather than
formulas.

## References

Richardson, T. S., Robins, J. M., & Wang, L. (2017). On modeling and
estimation for the relative risk and risk difference. Journal of the
American Statistical Association, 112(519), 1121–1130.
http://dx.doi.org/10.1080/01621459.2016.1192546

## Author

Klaus K. Holst

## Examples

``` r
m <- lava::lvm(a[-2] ~ x,
         z ~ 1,
         lp.target[1] ~ 1,
         lp.nuisance[-1] ~ 2*x) |>
     lava::distribution(~a, value=lava::binomial.lvm("logit")) |>
     lava::binomial.rr("y","a","lp.target","lp.nuisance")
d <- sim(m,5e2,seed=1)

I <- model.matrix(~1, d)
X <- model.matrix(~1+x, d)
with(d, riskreg_mle(y, a, I, X, type="rr"))
#>                          Estimate Std.Err    2.5%   97.5%   P-value
#> (Intercept)                0.9453  0.1150  0.7198  1.1707 2.092e-16
#> odds-product:(Intercept)  -0.8068  0.3031 -1.4009 -0.2126 7.783e-03
#> odds-product:x             2.3296  0.3914  1.5624  3.0969 2.659e-09

with(d, riskreg_fit(y, a, nuisance=X, propensity=I, type="rr"))
#>             Estimate Std.Err   2.5% 97.5%   P-value
#> (Intercept)   0.9469   0.113 0.7254 1.168 5.418e-17
riskreg(y ~ a | 1, nuisance=~x ,  data=d, type="rr")
#>             Estimate Std.Err   2.5% 97.5%   P-value
#> (Intercept)   0.9469   0.113 0.7254 1.168 5.418e-17

## Model with same design matrix for nuisance and propensity model:
with(d, riskreg_fit(y, a, nuisance=X, type="rr"))
#>             Estimate Std.Err   2.5% 97.5%   P-value
#> (Intercept)   0.9402  0.1173 0.7104  1.17 1.082e-15

## a <- riskreg(y ~ a, target=~z, nuisance=~x,
## propensity=~x, data=d, type="rr")
a <- riskreg(y ~ a | z, nuisance=~x,  propensity=~x, data=d, type="rr")
a
#>             Estimate Std.Err    2.5%     97.5%   P-value
#> (Intercept)   0.9797  0.1212  0.7421  1.217303 6.364e-16
#> z            -0.2218  0.1115 -0.4404 -0.003211 4.673e-02
predict(a, d[1:5,])
#> [1] 0.1491919 0.6897799 0.1040211 0.3736667 0.3304784

riskreg(y ~ a, nuisance=~x,  data=d, type="rr", mle=TRUE)
#>                          Estimate Std.Err    2.5%   97.5%   P-value
#> (Intercept)                0.9453  0.1150  0.7198  1.1707 2.092e-16
#> odds-product:(Intercept)  -0.8068  0.3031 -1.4009 -0.2126 7.783e-03
#> odds-product:x             2.3296  0.3914  1.5624  3.0969 2.659e-09
```
