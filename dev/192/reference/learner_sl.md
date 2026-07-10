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
#> Formula: y ~ 1 <environment: 0x561ef1707e10> 
#> ─────────────────────────────────────
#>          score     weight
#> mean 4.9077080 0.00000000
#> glm  0.9499446 0.04367903
#> iso  0.4804848 0.95632097
# weights(s$fit)
# score(s$fit)

cvres <- cv(s, data = d, nfolds = 3, rep = 2)
cvres
#> 
#> 3-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   0.53592 0.06583 0.45918 0.61290
#> mean 4.93582 0.25062 4.61359 5.20491
#> glm  0.95943 0.09491 0.80637 1.04611
#> iso  0.50016 0.06244 0.43834 0.60064
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.58451 0.03767 0.53719 0.63085
#> mean 1.78566 0.06822 1.68321 1.87589
#> glm  0.80297 0.04050 0.74230 0.85606
#> iso  0.55979 0.04080 0.51443 0.62650
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.07109 0.06884 0.00000 0.17930
#> glm  0.06627 0.02725 0.02534 0.10524
#> iso  0.86265 0.05976 0.76561 0.93646
# coef(cvres)
# score(cvres)
```
