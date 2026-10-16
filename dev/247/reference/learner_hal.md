# Construct a learner

Constructs a [learner](learner.md) class object for fitting a highly
adaptive lasso model with
[hal9001::fit_hal](https://rdrr.io/pkg/hal9001/man/fit_hal.html).

## Usage

``` r
learner_hal(
  formula,
  info = "hal9001::fit_hal",
  smoothness_orders = 0,
  reduce_basis = NULL,
  family = "gaussian",
  learner.args = NULL,
  ...
)
```

## Arguments

- formula:

  (formula) Formula specifying response and design matrix.

- info:

  (character) Optional information to describe the instantiated
  [learner](learner.md) object.

- smoothness_orders:

  An `integer`, specifying the smoothness of the basis functions. See
  details for `smoothness_orders` for more information.

- reduce_basis:

  Am optional `numeric` value bounded in the open unit interval
  indicating the minimum proportion of 1's in a basis function column
  needed for the basis function to be included in the procedure to fit
  the lasso. Any basis functions with a lower proportion of 1's than the
  cutoff will be removed. Defaults to 1 over the square root of the
  number of observations. Only applicable for models fit with zero-order
  splines, i.e. `smoothness_orders = 0`.

- family:

  A `character` or a [`family`](https://rdrr.io/r/stats/family.html)
  object (supported by
  [`glmnet`](https://rdrr.io/pkg/glmnet/man/glmnet.html)) specifying the
  error/link family for a generalized linear model. `character` options
  are limited to "gaussian" for fitting a standard penalized linear
  model, "binomial" for penalized logistic regression, "poisson" for
  penalized Poisson regression, "cox" for a penalized proportional
  hazards model, and "mgaussian" for multivariate penalized linear
  model. Note that passing in family objects leads to slower performance
  relative to passing in a character family (if supported). For example,
  one should set `family = "binomial"` instead of `family = binomial()`
  when calling `fit_hal`.

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to
  [hal9001::fit_hal](https://rdrr.io/pkg/hal9001/man/fit_hal.html).

## Value

[learner](learner.md) object.

## Examples

``` r
if (FALSE) { # \dontrun{
n <- 5e2
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
d <- data.frame(y, x1, x2)
lr <- learner_hal(y ~ x1 + x2, smoothness_orders = 0.5, reduce_basis = 1)
lr$estimate(d)
lr$predict(data.frame(x1 = 0, x2 = c(-1, 1)))
} # }
```
