# Construct a learner

Constructs a [learner](learner.md) class object for fitting entire lasso
or elastic-net regularization paths for various linear and non-linear
regression models with
[glmnet::cv.glmnet](https://glmnet.stanford.edu/reference/cv.glmnet.html).
Predictions are returned for the value of `lambda` that gives minimum
`cvm`. That is,
[glmnet::predict.cv.glmnet](https://glmnet.stanford.edu/reference/predict.cv.glmnet.html)
is called with `s = "lambda.min"`.

## Usage

``` r
learner_glmnet_cv(
  formula,
  info = "glmnet::cv.glmnet",
  family = gaussian(),
  lambda = NULL,
  alpha = 1,
  nfolds = 10,
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

- family:

  Either a character string representing one of the built-in families,
  or else a [`glm()`](https://rdrr.io/r/stats/glm.html) family object.
  For more information, see Details section below or the documentation
  for response type (above).

- lambda:

  Optional user-supplied lambda sequence; default is `NULL`, and
  `glmnet` chooses its own sequence. Note that this is done for the full
  model (master sequence), and separately for each fold. The fits are
  then alligned using the master sequence (see the `allignment` argument
  for additional details). Adapting `lambda` for each fold leads to
  better convergence. When `lambda` is supplied, the same sequence is
  used everywhere, but in some GLMs can lead to convergence issues.

- alpha:

  The elasticnet mixing parameter, with \\0\le\alpha\le 1\\. The penalty
  is defined as
  \$\$(1-\alpha)/2\|\|\beta\|\|\_2^2+\alpha\|\|\beta\|\|\_1.\$\$
  `alpha=1` is the lasso penalty, and `alpha=0` the ridge penalty.

- nfolds:

  number of folds - default is 10. Although `nfolds` can be as large as
  the sample size (leave-one-out CV), it is not recommended for large
  datasets. Smallest value allowable is `nfolds=3`

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Other arguments that can be passed to glmnet, for example `alpha`,
  `nlambda`, etc. See `glmnet` for details.

## Value

[learner](learner.md) object.

## Examples

``` r
# continuous outcome
n <- 5e2
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
lp <- x1 + x2*x1 + cos(x1)
y <- rnorm(n, lp, sd = 2)
d0 <- data.frame(y, x1, x2)

lr <- learner_glmnet_cv(y ~ x1 + x2)
lr$estimate(d0, nfolds = 3)
lr$predict(data.frame(x1 = c(0, 1), x2 = 1))
#> [1] 0.02458825 1.03957193

# count outcome with different exposure time
w <- 50 + rexp(n, rate = 1 / 5)
y <- rpois(n, exp(0.5 * x1 - 1 * x2 + log(w)) * rgamma(n, 1 / 2, 1 / 2))
d0 <- data.frame(y, x1, x2, w)

lr <- learner_glmnet_cv(y ~ x1 + x2 + offset(log(w)), family = "poisson")
lr$estimate(d0, nfolds = 3)
lr$predict(data.frame(x1 = 1, x2 = 1, w = c(1, 5)))
#> [1] 0.5722787 2.8613937
```
