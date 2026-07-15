# Cross-validation for [learner_sl](learner_sl.md)

Cross-validation estimation of the generalization error of the super
learner and each of the separate models in the ensemble. Both the chosen
model scoring metrics as well as the model weights of the stacked
ensemble.

## Usage

``` r
# S3 method for class 'learner_sl'
cv(object, data, nfolds = 5, rep = 1, model.score = scoring, ...)
```

## Arguments

- object:

  (learner_sl) Instantiated [learner_sl](learner_sl.md) object.

- data:

  data.frame or matrix

- nfolds:

  Number of folds (nfolds=0 simple test/train split into two folds
  1:(\[n\]/2), (\[n\]+1/2):n with last part used for testing)

- rep:

  Number of repetitions (default 1)

- model.score:

  Model scoring metric (default: MSE / Brier score). Must be a function
  with arguments response and prediction, and may optionally include
  weights, object and newdata arguments

- ...:

  Additional arguments parsed to elements in `object`

## Examples

``` r
sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
   data.frame(y, x1, x2)
}
sl <- learner_sl(list(
                   "mean" = learner_glm(y ~ 1),
                   "glm" = learner_glm(y ~ x1),
                   "glm2" = learner_glm(y ~ x1 + x2)
                  ))
cv(sl, data = sim1(), rep = 2)
#> 
#> 5-fold cross-validation with 2 repetitions
#> 
#> ── mse 
#>         mean      sd     min     max
#> sl   1.01431 0.08873 0.93515 1.22258
#> mean 5.42995 0.85016 4.13972 6.90850
#> glm  1.00524 0.07069 0.93515 1.14354
#> glm2 1.01062 0.06860 0.94143 1.14854
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.83286 0.04194 0.77691 0.90943
#> mean 1.81612 0.14066 1.61455 2.07032
#> glm  0.82996 0.03830 0.77240 0.89098
#> glm2 0.83231 0.03664 0.77926 0.89017
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.01414 0.04471 0.00000 0.14140
#> glm  0.89758 0.21042 0.34213 1.00000
#> glm2 0.08828 0.21225 0.00000 0.65787
```
