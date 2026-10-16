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
#> Formula: y ~ 1 <environment: 0x55b23ba27338> 
#> ─────────────────────────────────────
#>          score     weight
#> mean 4.9578321 0.04823261
#> glm  1.0475698 0.06115155
#> iso  0.5629416 0.89061584
# weights(s$fit)
# score(s$fit)

cvres <- cv(s, data = d, nfolds = 3, rep = 2)
cvres
#> 
#> 3-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   0.59370 0.03712 0.56110 0.64510
#> mean 4.94302 0.44086 4.54408 5.66172
#> glm  1.03810 0.09223 0.94897 1.18237
#> iso  0.57764 0.02353 0.53888 0.59745
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.60389 0.01796 0.58360 0.62675
#> mean 1.74329 0.10429 1.59815 1.91089
#> glm  0.82251 0.04321 0.77219 0.88703
#> iso  0.59329 0.01000 0.57897 0.60739
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.06934 0.02601 0.04933 0.11860
#> glm  0.07915 0.01034 0.06352 0.08850
#> iso  0.85152 0.02711 0.80969 0.88457
# coef(cvres)
# score(cvres)
```
