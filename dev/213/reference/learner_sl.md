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
#> Formula: y ~ 1 <environment: 0x55954770a350> 
#> ─────────────────────────────────────
#>         score     weight
#> mean 5.011196 0.06440568
#> glm  1.058765 0.08033375
#> iso  0.584849 0.85526057
# weights(s$fit)
# score(s$fit)

cvres <- cv(s, data = d, nfolds = 3, rep = 2)
cvres
#> 
#> 3-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   0.62401 0.04650 0.56521 0.67953
#> mean 5.01482 0.47872 4.43024 5.72789
#> glm  1.05843 0.08263 0.93481 1.18194
#> iso  0.59872 0.04513 0.53342 0.66822
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.62822 0.01889 0.60529 0.65086
#> mean 1.79246 0.04575 1.75501 1.87045
#> glm  0.82495 0.02906 0.78183 0.85334
#> iso  0.61530 0.01998 0.58478 0.64198
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.07765 0.06591 0.00000 0.18818
#> glm  0.08344 0.03393 0.04148 0.14000
#> iso  0.83891 0.06840 0.72423 0.91004
# coef(cvres)
# score(cvres)
```
