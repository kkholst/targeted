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
#> sl   0.94201 0.14798 0.68446 1.19622
#> mean 4.85148 0.76792 3.68066 6.09990
#> glm  0.93586 0.14178 0.67244 1.14837
#> glm2 0.94085 0.13464 0.69008 1.14560
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.78561 0.06666 0.67004 0.88270
#> mean 1.75670 0.11711 1.58373 1.88330
#> glm  0.78338 0.06551 0.66342 0.86603
#> glm2 0.78530 0.06339 0.67356 0.86630
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.00432 0.01365 0.00000 0.04317
#> glm  0.83094 0.34777 0.08865 1.00000
#> glm2 0.16475 0.34976 0.00000 0.91135
```
