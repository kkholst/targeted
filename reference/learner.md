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

## Methods

### Public methods

- [`learner$new()`](#method-learner-new)

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

### Method `new()`

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

- `intercept`:

  (logical) include intercept in design matrix

------------------------------------------------------------------------

### Method [`estimate()`](http://kkholst.github.io/lava/reference/estimate.default.md)

Estimation method

#### Usage

    learner$estimate(data, ..., store = TRUE)

#### Arguments

- `data`:

  data.frame

- `...`:

  Additional arguments to estimation method

- `store`:

  Logical determining if estimated model should be stored inside the
  class.

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Prediction method

#### Usage

    learner$predict(newdata, ..., object = NULL)

#### Arguments

- `newdata`:

  data.frame

- `...`:

  Additional arguments to prediction method

- `object`:

  Optional model fit object

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update formula

#### Usage

    learner$update(formula)

#### Arguments

- `formula`:

  formula or character which defines the new response

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    learner$print()

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

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

### Method `response()`

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

### Method [`design()`](design.md)

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

### Method `opt()`

Get options

#### Usage

    learner$opt(arg)

#### Arguments

- `arg`:

  name of option to get value of

------------------------------------------------------------------------

### Method `clone()`

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
#>         setosa  versicolor   virginica
#> [1,] 0.9823929 0.014178571 0.003428571
#> [2,] 0.9510833 0.043488095 0.005428571
#> [3,] 0.9844048 0.013095238 0.002500000
#> [4,] 0.9687738 0.028726190 0.002500000
#> [5,] 0.9898929 0.006678571 0.003428571
#> [6,] 0.9064936 0.068506410 0.025000000

# \donttest{
# Reduce Ex. timing
a <- targeted::cv(models, data = iris)
cbind(coef(a), attr(args, "table"))
#>              brier -logscore
#> model1  0.10005912 0.2205740
#> model2  0.10032820 0.2232710
#> model3  0.08405392 0.1823416
#> model4  0.08297912 0.1784703
#> model5  0.08659444 0.1799173
#> model6  0.08517396 0.1764445
#> model7  0.35036559 0.5715464
#> model8  0.34923672 0.5664500
#> model9  0.34400827 0.5539763
#> model10 0.34324098 0.5507146
#> model11 0.34244349 0.5466884
#> model12 0.34120762 0.5492056
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
## Method `learner$summary`
## ------------------------------------------------

lr <- learner_glm(y ~ x, family = "nb")
lr$summary()
#> ────────── learner object ──────────
#> glm 
#> 
#> formula: y ~ x <environment: 0x563cbf7048e0> 
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
#> formula: y ~ x <environment: 0x563cbf7048e0> 
#> estimate: formula, data, family, ... 
#> estimate.args: family=nb 
#> predict: object, newdata, ... 
#> predict.args:   
#> specials:  
```
