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
  vector), `y`, and the predictions (nxp matrix), `pred`, with p being
  the number of learners. Alternatively, this can be set to the
  character value "discrete", in which case the Discrete Super-Learner
  is applied where the model with the lowest risk (model-score) is given
  weight 1 and all other learners weight 0.

- model.score:

  (function) Model scoring method (see [learner](learner.md))

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
#> Formula: y ~ 1 <environment: 0x563cc22f82b8> 
#> ─────────────────────────────────────
#>          score     weight
#> mean 4.8684025 0.09195207
#> glm  0.8579904 0.07689746
#> iso  0.4668761 0.83115047
# weights(s$fit)
# score(s$fit)

cvres <- cv(s, data = d, nfolds = 3, rep = 2)
cvres
#> 
#> 3-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   0.49724 0.04312 0.45503 0.55521
#> mean 4.86631 0.29296 4.55459 5.23194
#> glm  0.86610 0.05665 0.76497 0.91749
#> iso  0.47305 0.01645 0.44241 0.49028
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.56817 0.02602 0.54496 0.61206
#> mean 1.76295 0.06099 1.66893 1.85225
#> glm  0.75561 0.03516 0.69341 0.79557
#> iso  0.55661 0.01008 0.54620 0.56973
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.07748 0.04056 0.02455 0.11796
#> glm  0.09614 0.01524 0.07534 0.12235
#> iso  0.82638 0.04238 0.78781 0.87947
# coef(cvres)
# score(cvres)
```
