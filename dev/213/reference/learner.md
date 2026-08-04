# R6 class for prediction models

Interface for statistical and machine learning models to be used for
nuisance model estimation in targeted learning.

The following list provides an overview of constructors for many
commonly used models.

Regression and classification: [learner_glm](learner_glm.md),
[learner_gam](learner_gam.md), [learner_grf](learner_grf.md),
[learner_hal](learner_hal.md),
[learner_glmnet_cv](learner_glmnet_cv.md),
[learner_svm](learner_svm.md), [learner_xgboost](learner_xgboost.md),
[learner_mars](learner_mars.md)  
Regression: [learner_isoreg](learner_isoreg.md)  
Classification: [learner_naivebayes](learner_naivebayes.md)  
Ensemble (super learner): [learner_sl](learner_sl.md)

The following constructors for commonly used filters are available:
[predict_filter_bound](predict_filter_bound.md),
[predict_filter_bound_dynamic](predict_filter_bound_dynamic.md)

## Author

Klaus Kähler Holst, Benedikt Sommer

## Public fields

- `info`:

  Optional information/name of the model

## Active bindings

- `clear`:

  Remove fitted model from the learner object

- `fit`:

  Return estimated model object.

- `formula`:

  Return model formula. Use learner\$update() to update the formula.

- `predict.filter`:

  Return instantiated prediction filter function

- `predict.filter.generator`:

  Return prediction filter generator function

## Methods

### Public methods

- [`learner$new()`](#method-learner-initialize)

- [`learner$estimate()`](#method-learner-estimate)

- [`learner$predict()`](#method-learner-predict)

- [`learner$update()`](#method-learner-update)

- [`learner$print()`](#method-learner-print)

- [`learner$summary()`](#method-learner-summary)

- [`learner$response()`](#method-learner-response)

- [`learner$design()`](#method-learner-design)

- [`learner$opt()`](#method-learner-opt)

- [`learner$clone()`](#method-learner-clone)

------------------------------------------------------------------------

### `learner$new()`

Create a new prediction model object

#### Usage

    learner$new(
      formula = NULL,
      estimate,
      predict = stats::predict,
      predict.args = NULL,
      estimate.args = NULL,
      info = NULL,
      specials = c(),
      formula.keep.specials = FALSE,
      predict.filter = function(data) function(pred, newdata) pred,
      intercept = FALSE
    )

#### Arguments

- `formula`:

  formula specifying outcome and design matrix

- `estimate`:

  function for fitting the model. This must be a function with response,
  'y', and design matrix, 'x'. Alternatively, a function with a formula
  and data argument. See the examples section.

- `predict`:

  prediction function (must be a function of model object, 'object', and
  new design matrix, 'newdata')

- `predict.args`:

  optional arguments to prediction function

- `estimate.args`:

  optional arguments to estimate function

- `info`:

  optional description of the model

- `specials`:

  optional specials terms (weights, offset, id, subset, ...) passed on
  to [design](design.md)

- `formula.keep.specials`:

  if TRUE then special terms defined by `specials` will be removed from
  the formula before it is being passed to the estimate print.function()

- `predict.filter`:

  function to post-process predictions. Useful to bound predictions or
  handle NAs. The argument is experimental and its behavior may change
  in the future.

- `intercept`:

  (logical) include intercept in design matrix

------------------------------------------------------------------------

### `learner$estimate()`

Estimation method

#### Usage

    learner$estimate(data, ..., store = TRUE)

#### Arguments

- `data`:

  data.frame

- `...`:

  Additional arguments to estimation and prediction filter generator
  function

- `store`:

  Logical determining if estimated model should be stored inside the
  class.

------------------------------------------------------------------------

### `learner$predict()`

Prediction method

#### Usage

    learner$predict(newdata, ..., object = NULL)

#### Arguments

- `newdata`:

  data.frame

- `...`:

  Additional arguments to prediction method and prediction filter
  function

- `object`:

  Optional model fit object

------------------------------------------------------------------------

### `learner$update()`

Update formula

#### Usage

    learner$update(formula)

#### Arguments

- `formula`:

  formula or character which defines the new response

------------------------------------------------------------------------

### `learner$print()`

Print method

#### Usage

    learner$print()

------------------------------------------------------------------------

### `learner$summary()`

Summary method to provide more extensive information than
learner\$print().

#### Usage

    learner$summary()

#### Returns

summarized_learner object, which is a list with the following elements:

- info:

  description of the learner

- formula:

  formula specifying outcome and design matrix

- estimate:

  function for fitting the model

- estimate.args:

  arguments to estimate function

- predict:

  function for making predictions from fitted model

- predict.args:

  arguments to predict function

- specials:

  provided special terms

- intercept:

  include intercept in design matrix

#### Examples

    lr <- learner_glm(y ~ x, family = "nb")
    lr$summary()

    lr_sum <- lr$summary() # store returned summary in new object
    names(lr_sum)
    print(lr_sum)

------------------------------------------------------------------------

### `learner$response()`

Extract response from data

#### Usage

    learner$response(data, eval = TRUE, ...)

#### Arguments

- `data`:

  data.frame

- `eval`:

  when FALSE return the untransformed outcome (i.e., return 'a' if
  formula defined as I(a==1) ~ ...)

- `...`:

  additional arguments to [design](design.md)

------------------------------------------------------------------------

### `learner$design()`

Generate [design](design.md) object (design matrix and response) from
data

#### Usage

    learner$design(data, ...)

#### Arguments

- `data`:

  data.frame

- `...`:

  additional arguments to [design](design.md)

------------------------------------------------------------------------

### `learner$opt()`

Get options

#### Usage

    learner$opt(arg)

#### Arguments

- `arg`:

  name of option to get value of

------------------------------------------------------------------------

### `learner$clone()`

The objects of this class are cloneable with this method.

#### Usage

    learner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data(iris)
rf <- function(formula, ...) {
  learner$new(formula,
    info = "grf::probability_forest",
    estimate = function(x, y, ...) {
      grf::probability_forest(X = x, Y = y, ...)
    },
    predict = function(object, newdata) {
      predict(object, newdata)$predictions
    },
    estimate.args = list(...)
  )
}

args <- expand.list(
  num.trees = c(100, 200), mtry = 1:3,
  formula = c(Species ~ ., Species ~ Sepal.Length + Sepal.Width)
)
models <- lapply(args, function(par) do.call(rf, par))

x <- models[[1]]$clone()
x$estimate(iris)
predict(x, newdata = head(iris))
#>         setosa versicolor   virginica
#> [1,] 0.9975000 0.00250000 0.000000000
#> [2,] 0.9552273 0.02872727 0.016045455
#> [3,] 0.9793333 0.01233333 0.008333333
#> [4,] 0.9570000 0.03550000 0.007500000
#> [5,] 0.9975000 0.00250000 0.000000000
#> [6,] 0.9457630 0.04592352 0.008313492

# \donttest{
# Reduce Ex. timing
a <- targeted::cv(models, data = iris)
cbind(coef(a), attr(args, "table"))
#>              brier -logscore
#> model1  0.09905229 0.2156259
#> model2  0.09770097 0.2155833
#> model3  0.09027414 0.1869423
#> model4  0.08930044 0.1884777
#> model5  0.08507715 0.1677289
#> model6  0.08174147 0.1697481
#> model7  0.32596732 0.5374138
#> model8  0.33396408 0.5485199
#> model9  0.32949550 0.5414416
#> model10 0.33061998 0.5382947
#> model11 0.32393173 0.5271123
#> model12 0.32291826 0.5244176
# }

# defining learner via function with arguments y (response)
# and x (design matrix)
f1 <- learner$new(
  estimate = function(y, x) lm.fit(x = x, y = y),
  predict = function(object, newdata) newdata %*% object$coefficients
)
# defining the learner via arguments formula and data
f2 <- learner$new(
  estimate = function(formula, data, ...) glm(formula, data, ...)
)
# generic learner defined from function (predict method derived per default
# from stats::predict
f3 <- learner$new(
  estimate = function(dt, ...) {
    lm(y ~ x, data = dt)
  }
)

## ------------------------------------------------
## Method `learner$summary()`
## ------------------------------------------------

lr <- learner_glm(y ~ x, family = "nb")
lr$summary()
#> ────────── learner object ──────────
#> glm 
#> 
#> formula: y ~ x <environment: 0x5602480921c0> 
#> estimate: formula, data, family, ... 
#> estimate.args: family=nb 
#> predict: object, newdata, ... 
#> predict.args:   
#> specials:  

lr_sum <- lr$summary() # store returned summary in new object
names(lr_sum)
#> [1] "formula"       "info"          "estimate.args" "predict.args" 
#> [5] "estimate"      "predict"       "specials"      "intercept"    
print(lr_sum)
#> ────────── learner object ──────────
#> glm 
#> 
#> formula: y ~ x <environment: 0x5602480921c0> 
#> estimate: formula, data, family, ... 
#> estimate.args: family=nb 
#> predict: object, newdata, ... 
#> predict.args:   
#> specials:  
```
