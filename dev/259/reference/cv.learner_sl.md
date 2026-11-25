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
#> sl   0.98171 0.17112 0.76451 1.31644
#> mean 5.23341 0.47778 4.40697 6.08616
#> glm  0.97356 0.15451 0.76418 1.23471
#> glm2 0.97557 0.15381 0.77107 1.23388
#> 
#> ── mae 
#>         mean      sd     min     max
#> sl   0.80099 0.07512 0.69462 0.94047
#> mean 1.81969 0.10473 1.59447 1.95995
#> glm  0.79720 0.06820 0.69437 0.90354
#> glm2 0.79753 0.06721 0.69862 0.90370
#> 
#> ── weight 
#>         mean      sd     min     max
#> sl         -       -       -       -
#> mean 0.00852 0.02393 0.00000 0.07614
#> glm  0.98546 0.02866 0.92386 1.00000
#> glm2 0.00603 0.01905 0.00000 0.06025
```
