# Construct a learner

Constructs a [learner](learner.md) class object for fitting a
[superlearner](superlearner.md).

## Usage

``` r
learner_sl(
  learners,
  info = NULL,
  nfolds = 5L,
  meta.learner = metalearner_nnls,
  model.score = mse,
  learner.args = NULL,
  ...
)
```

## Arguments

- learners:

  (list) List of [learner](learner.md) objects (i.e.
  [learner_glm](learner_glm.md))

- info:

  (character) Optional information to describe the instantiated
  [learner](learner.md) object.

- nfolds:

  (integer) Number of folds to use in cross-validation to estimate the
  ensemble weights.

- meta.learner:

  (function) Algorithm to learn the ensemble weights (default
  non-negative least squares). Must be a function of the response (nx1
  vector), `y`, and the base learner predictions (nxp matrix), `pred`,
  with p being the number of learners. The function can optionally
  accept a `model.score` argument for scoring the base learners. See
  [metalearner_nnls](metalearner_nnls.md),
  [metalearner_convexcomb](metalearner_convexcomb.md) and
  [metalearner_discrete](metalearner_discrete.md) for the available meta
  learners.

- model.score:

  (function) Method for scoring the predictions of each base learner.
  Expects two arguments; vector of response variable and prediction from
  a base learner (see `targeted:::mse` for additional details).

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to [superlearner](superlearner.md)

## Value

[learner](learner.md) object.

## See also

[cv.learner_sl](cv.learner_sl.md)

## Examples

``` r
sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
   data.frame(y, x1, x2)
}
d <- sim1()

m <- list(
  "mean" = learner_glm(y ~ 1),
  "glm" = learner_glm(y ~ x1 + x2),
  "iso" = learner_isoreg(y ~ x1)
)

s <- learner_sl(m, nfolds = 10)
s$estimate(d)
pr <- s$predict(d)
if (interactive()) {
    plot(y ~ x1, data = d)
    points(d$x1, pr, col = 2, cex = 0.5)
    lines(cos(x1) + x1 ~ x1, data = d[order(d$x1), ],
          lwd = 4, col = lava::Col("darkblue", 0.3))
}
print(s)
#> ────────── learner object ──────────
#> superlearner
#>  mean
#>  glm
#>  iso 
#> 
#> Estimate arguments: learners=<list>, nfolds=10, meta.learner=<function>, model.score=<function> 
#> Predict arguments:   
#> Formula: y ~ 1 <environment: 0x559ffca2dd38> 
#> ─────────────────────────────────────
#>          score     weight
#> mean 4.6075512 0.03921874
#> glm  0.9789936 0.03968460
#> iso  0.4999095 0.92109667
# weights(s$fit)
# score(s$fit)

cvres <- cv(s, data = d, nfolds = 3, rep = 2)
cvres
#> 
#> 3-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   0.53129 0.03245 0.49217 0.56774
#> mean 4.59657 0.50115 3.70142 5.07191
#> glm  0.97602 0.07337 0.88417 1.06816
#> iso  0.52010 0.03172 0.47648 0.56542
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.58651 0.02967 0.54094 0.61505
#> mean 1.65086 0.11119 1.47531 1.74119
#> glm  0.79803 0.02926 0.76318 0.83569
#> iso  0.58060 0.02988 0.52930 0.61327
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.05328 0.02734 0.01802 0.08715
#> glm  0.07052 0.00591 0.06439 0.08101
#> iso  0.87621 0.02468 0.84846 0.90740
# coef(cvres)
# score(cvres)
```
