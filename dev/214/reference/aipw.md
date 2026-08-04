# AIPW estimator

AIPW for the mean (and linear projections of the EIF) with missing
observations

## Usage

``` r
aipw(
  response.model,
  propensity.model,
  formula = ~1,
  data,
  response_model = deprecated,
  propensity_model = deprecated,
  ...
)
```

## Arguments

- response.model:

  Model for the response given covariates (learner or formula)

- propensity.model:

  Optional missing data mechanism model (propensity model) (learner or
  formula)

- formula:

  design specifying the OLS estimator with outcome given by the EIF (see
  `cate`)

- data:

  data.frame

- response_model:

  Deprecated. Use response.model instead.

- propensity_model:

  Deprecated. Use treatment.model instead.

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
