# AIPW estimator

AIPW for the mean (and linear projections of the EIF) with missing
observations

## Usage

``` r
aipw(response_model, propensity_model, formula = ~1, data, ...)
```

## Arguments

- response_model:

  Model for the response given covariates (learner or formula)

- propensity_model:

  Optional missing data mechanism model (propensity model) (learner or
  formula)

- formula:

  design specifying the OLS estimator with outcome given by the EIF

- data:

  data.frame

- ...:

  additional arguments (see [`cate()`](cate.md))

## Examples

``` r
m <- lava::lvm(y ~ x+z, r ~ x) |>
     lava::distribution(~ r, value = lava::binomial.lvm()) |>
     transform(y0~r+y, value = \(x) { x[x[,1]==0,2] <- NA; x[,2] })
d <- lava::sim(m,1e3,seed=1)

aipw(y0 ~ x, data=d)
#>             Estimate Std.Err    2.5%  97.5% P-value
#> (Intercept)  0.05148 0.08338 -0.1119 0.2149   0.537
```
