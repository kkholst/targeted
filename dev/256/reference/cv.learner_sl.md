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
#> sl   1.11649 0.19249 0.83370 1.44081
#> mean 4.87269 0.45380 4.18915 5.58744
#> glm  1.11573 0.19186 0.83370 1.43625
#> glm2 1.11585 0.19229 0.83560 1.44622
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.86937 0.07668 0.75694 0.97436
#> mean 1.71915 0.09323 1.58664 1.85736
#> glm  0.86879 0.07611 0.75694 0.97005
#> glm2 0.86932 0.07804 0.75570 0.97815
#> 
#> ── weight 
#>        mean      sd     min     max
#> sl        -       -       -       -
#> mean 0.0000 0.00000 0.00000 0.00000
#> glm  0.8789 0.25632 0.34602 1.00000
#> glm2 0.1211 0.25632 0.00000 0.65398
```
