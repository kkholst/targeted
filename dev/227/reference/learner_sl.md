# Construct a learner

Constructs a [learnerSL](learnerSL.md) object for fitting a
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

[learnerSL](learnerSL.md) object.

## See also

[learnerSL](learnerSL.md), [cv.learner_sl](cv.learner_sl.md)

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
#> Estimate arguments: nfolds=10, meta.learner=<function>, model.score=<function> 
#> Predict arguments:   
#> Formula: y ~ 1 <environment: 0x55b7250d9420> 
#> ─────────────────────────────────────
#>          score     weight
#> mean 4.5101604 0.03246338
#> glm  1.0593160 0.06283154
#> iso  0.5681082 0.90470508
# weights(s$fit)
# score(s$fit)

cvres <- cv(s, data = d, nfolds = 3, rep = 2)
cvres
#> 
#> 3-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   0.59424 0.03582 0.54221 0.63345
#> mean 4.51392 0.24266 4.12502 4.78271
#> glm  1.06691 0.08806 0.92500 1.19642
#> iso  0.58440 0.03573 0.51989 0.61871
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.60215 0.01226 0.58744 0.61837
#> mean 1.64096 0.05957 1.58145 1.74909
#> glm  0.82839 0.03903 0.76473 0.88307
#> iso  0.59698 0.01298 0.57424 0.60792
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.03984 0.02613 0.00152 0.07432
#> glm  0.08342 0.03599 0.02634 0.11929
#> iso  0.87674 0.03991 0.82968 0.92464
# coef(cvres)
# score(cvres)
```
