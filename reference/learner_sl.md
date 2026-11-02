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
#> Formula: y ~ 1 <environment: 0x63cf5c8f0f68> 
#> ─────────────────────────────────────
#>          score    weight
#> mean 5.2979078 0.0313786
#> glm  1.0645924 0.0600804
#> iso  0.5466697 0.9085410
# weights(s$fit)
# score(s$fit)

cvres <- cv(s, data = d, nfolds = 3, rep = 2)
cvres
#> 
#> 3-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   0.58765 0.06936 0.50615 0.70088
#> mean 5.32158 0.34920 4.92775 5.93334
#> glm  1.06505 0.08888 0.93050 1.19726
#> iso  0.56888 0.03924 0.50105 0.61834
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.61449 0.03266 0.57200 0.66548
#> mean 1.84504 0.07129 1.72977 1.94877
#> glm  0.82578 0.03890 0.77602 0.88512
#> iso  0.60613 0.02583 0.56410 0.64054
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.05846 0.05056 0.00000 0.12857
#> glm  0.08531 0.01825 0.05465 0.10126
#> iso  0.85623 0.05248 0.77017 0.91376
# coef(cvres)
# score(cvres)
```
