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
#> sl   0.97783 0.10834 0.84327 1.16670
#> mean 4.83617 0.55126 3.76855 5.61918
#> glm  0.93604 0.10606 0.81326 1.15954
#> glm2 0.93807 0.10821 0.81342 1.16182
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.79538 0.04366 0.72901 0.85986
#> mean 1.74969 0.10167 1.54912 1.88131
#> glm  0.78390 0.05189 0.71345 0.87016
#> glm2 0.78475 0.05216 0.71386 0.86993
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.04846 0.09221 0.00000 0.24134
#> glm  0.88012 0.17292 0.47291 1.00000
#> glm2 0.07141 0.17056 0.00000 0.52709
```
