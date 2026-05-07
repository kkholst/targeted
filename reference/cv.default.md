# Cross-validation

Generic cross-validation function

## Usage

``` r
# Default S3 method
cv(
  object,
  data,
  response = NULL,
  nfolds = 5,
  rep = 1,
  weights = NULL,
  model.score = scoring,
  seed = NULL,
  shared = NULL,
  args.pred = NULL,
  args.future = list(),
  mc.cores,
  silent = FALSE,
  ...
)
```

## Arguments

- object:

  List of [learner](learner.md) objects

- data:

  data.frame or matrix

- response:

  Response variable (vector or name of column in `data`).

- nfolds:

  Number of folds (nfolds=0 simple test/train split into two folds
  1:(\[n\]/2), (\[n\]+1/2):n with last part used for testing)

- rep:

  Number of repetitions (default 1)

- weights:

  Optional frequency weights

- model.score:

  Model scoring metric (default: MSE / Brier score). Must be a function
  with arguments response and prediction, and may optionally include
  weights, object and newdata arguments

- seed:

  Random seed (argument parsed to future_Apply::future_lapply)

- shared:

  Function applied to each fold with results send to each model

- args.pred:

  Optional arguments to prediction function (see details below)

- args.future:

  Arguments to future.apply::future_mapply

- mc.cores:

  Optional number of cores.
  [parallel::mcmapply](https://rdrr.io/r/parallel/mclapply.html) used
  instead of future

- silent:

  suppress all messages and progressbars

- ...:

  Additional arguments parsed to elements in `object`

## Value

An object of class '`cross_validated`' is returned. See
[`cross_validated-class`](cross_validated-class.md) for more details
about this class and its generic functions.

## Details

`object` should be list of objects of class [learner](learner.md).
Alternatively, each element of models should be a list with a fitting
function and a prediction function.

The `response` argument can optionally be a named list where the name is
then used as the name of the response argument in models. Similarly, if
data is a named list with a single data.frame/matrix then this name will
be used as the name of the data/design matrix argument in models.

## See also

[cv.learner_sl](cv.learner_sl.md)

## Author

Klaus K. Holst

## Examples

``` r
m <- list(learner_glm(Sepal.Length~1),
          learner_glm(Sepal.Length~Species),
          learner_glm(Sepal.Length~Species + Petal.Length))
x <- cv(m, rep=10, data=iris)
x
#> Call: cv.default(object = m, data = iris, rep = 10)
#> 
#> 5-fold cross-validation with 10 repetitions
#> 
#>              mse       mae
#> model1 0.6911157 0.6923267
#> model2 0.2701731 0.4062745
#> model3 0.1175461 0.2769748
```
