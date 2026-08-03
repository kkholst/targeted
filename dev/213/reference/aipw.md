# AIPW estimator

AIPW for the mean (and linear projections of the EIF) with missing
observations

## Usage

``` r
aipw(response.model, propensity.model, formula = ~1, data, ...)
```

## Arguments

- response.model:

  (learner or formula)Model for the response given covariates

- propensity.model:

  (learner or formula) missing data mechanism model and if omitted a
  logistic regression model with the same covariates as `response.model`
  is used

- formula:

  design specifying the OLS estimator with outcome given by the EIF (see
  `cate`)

- data:

  data.frame

- ...:

  additional arguments (see [`cate()`](cate.md))

## Examples

``` r
m <- lava::lvm(y ~ x+z, r ~ x) |>
     lava::distribution(~ r, value = lava::binomial.lvm()) |>
     transform(y0~r+y, value = \(x) { x[x[,1]==0,2] <- NA; x[,2] })
d <- lava::sim(m,5e3,seed=1)

aipw(y0 ~ x, ~ x + z, data=d)
#>             Estimate Std.Err     2.5%   97.5% P-value
#> (Intercept) -0.02208 0.03092 -0.08269 0.03852  0.4751
```
