# Prediction model class (`learner`)

## Introduction

Methods for targeted and semiparametric inference rely on fitting
nuisance models to observed data when estimating the target parameter of
interest. The [targeted](https://kkholst.github.io/targeted/) package
implements the [R6 reference class](https://r6.r-lib.org/) `learner` to
harmonize common statistical and machine learning models for the usage
as nuisance models across the various implemented estimators. Commonly
used models are constructed as `learner` class objects through the
`learner_` functions. These functions are wrappers for statistical and
machine learning models that have been implemented in other R packages.
This is conceptually similar to packages such as
[caret](https://github.com/topepo/caret/) and
[mlr3](https://mlr3.mlr-org.com). Besides implementing a large number of
prediction models, these packages provide extensive functionalities for
data preprocessing, model selection and diagnostics, and hyper-parameter
optimization. The design of the `learner` class and accompanied
functions is much leaner, as our use-cases often only require a
standardized interface for estimating models and generating predictions,
instead of the full predictive modelling pipeline.

This vignette uses the Mayo Clinic Primary Biliary Cholangitis data
([`?survival::pbc`](https://rdrr.io/pkg/survival/man/pbc.html)) to
exemplify the usage of the `learner` class objects. Our interest lies in
the prediction of the composite event of death or transplant before 2
years.

``` r

data(pbc, package="survival")
pbc <- transform(pbc, y = (time < 730) * (status > 0))
```

A logistic regression model with a single `age` covariate is defined and
estimated via

``` r

lr <- learner_glm(y ~ age, family = binomial())
lr$estimate(pbc)
```

and predictions for the event `y = 1` (class 2 probabilities) for new
data are obtained by

``` r

lr$predict(newdata = data.frame(age = c(20, 40, 60, 80)))
```

             1          2          3          4
    0.02578001 0.06868011 0.17047725 0.36415988 

The remainder of this vignette is structured as follows. We first
provide more details about the built-in learners that wrap statistical
and machine learning models from other R packages. Once the usage of the
returned `learner` class objects has been introduced, we provide details
to implement new learners.

## Built-in learners

The various constructors for commonly used statistical and machine
learning models are listed as part of the `learner` class documentation

``` r

?learner # help(learner)
```

which contains all essential information for the usage of the `learner`
class objects. With the exception of `learner_sl`, all constructors
require a `formula` argument that specify the response vector and design
matrix for the underlying model. This way the construction of a learner
is separated from the estimation of the model, as shown in the above
logistic regression example. A result of this design is the convenient
specification of ensemble learners (superlearner), where individual
learners operate on different subsets of features.

``` r

lr_sl <- learner_sl(
  learners = list(
    glm1 = learner_glm(y ~ age * bili, family = "binomial"),
    glm2 = learner_glm(y ~ age, family = "binomial"),
    gam = learner_gam(y ~ s(age) + s(bili), family = "binomial")
  )
)
lr_sl$estimate(pbc, nfolds = 10)
lr_sl
```

    ────────── learner object ──────────
    superlearner
        glm1
        glm2
        gam

    Estimate arguments: learners=<list>, nfolds=5, meta.learner=<function>, model.score=<function>
    Predict arguments:
    Formula: y ~ age * bili
    ─────────────────────────────────────
              score     weight
    glm1 0.09745565 0.31289733
    glm2 0.10730394 0.04260906
    gam  0.09574632 0.64449361

Most constructors have additional arguments that impact the resulting
model fit, ranging from the specification of a link function for
generalized linear models to hyper hyperparameters of machine learning
models. Arguments naturally vary between the constructors and are
documented for each function (e.g.
[`?learner_gam`](../reference/learner_gam.md)). The implemented
constructors only provide arguments for the most relevant parameters of
the underlying fit function
([`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) in case of
`learner_gam`). As described in the documentation, the ellipsis argument
(`...`) can be used to pass additional arguments to the fit function.
However, in most situations this is rarely required.

It is often necessary to consider a range of models with different
hyper-parameters and to facilitate this we can use the
`learner_expand_grid` function

``` r

lrs <- learner_expand_grid( learner_xgboost,
                            list(formula = Sepal.Length ~ .,
                                eta = c(0.2, 0.5, 0.3)) )
lrs
```

    $`xgboost reg:squarederror`
    ────────── learner object ──────────
    xgboost reg:squarederror

    Estimate arguments: max_depth=2, learning_rate=1, nrounds=2, subsample=1, reg_lambda=1, objective=reg:squarederror, eta=0.2
    Predict arguments:
    Formula: Sepal.Length ~ .

    $`xgboost reg:squarederror.1`
    ────────── learner object ──────────
    xgboost reg:squarederror

    Estimate arguments: max_depth=2, learning_rate=1, nrounds=2, subsample=1, reg_lambda=1, objective=reg:squarederror, eta=0.5
    Predict arguments:
    Formula: Sepal.Length ~ .

    $`xgboost reg:squarederror.2`
    ────────── learner object ──────────
    xgboost reg:squarederror

    Estimate arguments: max_depth=2, learning_rate=1, nrounds=2, subsample=1, reg_lambda=1, objective=reg:squarederror, eta=0.3
    Predict arguments:
    Formula: Sepal.Length ~ . 

The list of models can then serve as inputs for `predictor_sl` or for
assessing the generalization error of the model across different
hyper-parameters with the `cv` method.

## Usage

The basic usage is to construct a new learner by providing the `formula`
argument and the set of additional parameters that control the model
fitting process and the task (binary classification in this example).

``` r

lr_xgboost <- learner_xgboost(
  formula = y ~ age + sex + bili,
  eta = 0.3, nrounds = 5,  # hyperparameters
  objective = "binary:logistic" # learning task
)
lr_xgboost
```

    ────────── learner object ──────────
    xgboost binary:logistic

    Estimate arguments: max_depth=2, learning_rate=1, nrounds=5, subsample=1, reg_lambda=1, objective=binary:logistic, eta=0.3
    Predict arguments:
    Formula: y ~ age + sex + bili 

The model is estimated via the `estimate` method

``` r

lr_xgboost$estimate(data = pbc)
```

The default behavior is to assign the fitted model to the learner
object, which can be accessed via

``` r

class(lr_xgboost$fit)
```

    [1] "xgb.Booster"

Once the model has been fitted, predictions are generated with

``` r

lr_xgboost$predict(head(pbc))
```

    [1] 0.66811311 0.10537987 0.32460621 0.10537987 0.11731242 0.03387715

where the fitted model is used inside the learner object to generate the
predictions.

S3 methods are implemented for the `learner` class objects as an
alternative to the R6 class syntax for fitting the model and making
predictions.

``` r

lr <- learner_glm(y ~ age, family = "binomial")
estimate(lr, pbc)
predict(lr, head(pbc))
```

             1          2          3          4          5          6
    0.16171479 0.14624529 0.25614862 0.13566571 0.06272415 0.22071201 

### Update formula

The estimate and predict functions of `learner` class objects are by
design immutable. Both functions can be inspected as part of the return
values of the summary method

``` r

lr_xgboost$summary()$estimate
```

    function(x, y, ...) {
          params <- list(...)
          par1 <- intersect(formalArgs(xgboost::xgb.params), names(params))
          xgb_params <- params[par1]
          xgb_train_args <- params
          xgb_train_args[par1] <- NULL
          d <- xgboost::xgb.DMatrix(x, label = y)
          res <- do.call(
            xgboost::xgb.train,
            c(list(data = d), xgb_train_args, list(params = xgb_params)),
            )
          return(res)
        }
    <bytecode: 0x5652e4101e58>
    <environment: 0x5652e72abaf0>

Rare situations may arise where one wants to update the formula
argument. This supported and implemented via the `update` method

``` r

lr_xgboost$update(y ~ age + sex)
```

The design matrix that results from by the specified formula can be
inspected via

``` r

head(lr_xgboost$design(pbc)$x)
```

           age sexf
    1 58.76523    1
    2 56.44627    1
    3 70.07255    0
    4 54.74059    1
    5 38.10541    1
    6 66.25873    1

and the response vector via

``` r

head(lr_xgboost$response(pbc))
```

    1 2 3 4 5 6
    1 0 0 0 0 0 

See [`?learner`](../reference/learner.md) for the differences between
`learner$design` and `learner$response` and
[`?design`](../reference/design.md) for more details about the
construction of the design matrix and response vector.

### Cross-validation

The [targeted](https://kkholst.github.io/targeted/) package provides a
generic implementation for repeated k-fold cross-validation with
`learner` class objects. Parallelization is supported via the
[future](https://future.futureverse.org) and `{parallel}` packages (see
[`?cv`](../reference/cv.default.md) for more details).

``` r

# future::plan("multicore")
lrs <- list(
  glm = learner_glm(y ~ age + age, family = "binomial"),
  gam = learner_gam(y ~ s(age) + s(bili), family = "binomial")
)
 # 2 times repeated 5-fold cross-validation
cv(lrs, data = pbc, rep = 2, nfolds = 5)
```

    Call: cv.default(object = lrs, data = pbc, nfolds = 5, rep = 2)

    5-fold cross-validation with 2 repetitions

               mse       mae
    glm 0.10741276 0.2125264
    gam 0.09744471 0.1882385

### Prediction filter

The experimental `predict.filter` argument allows post-processing of
predictions, for example, to prevent predictions with extreme values or
to handle rows for which the prediction function fails.

``` r

n <- 1e3
d0 <- data.frame(x = rnorm(n))
d0$y <- with(d0, x + rnorm(n))

filter_bound <- function(data) {
  function(pred, newdata) {
    pred[pred > 10] <- 10
    pred
  }
}

lr_bound <- learner_glm(
  y ~ x,
  learner.args = list(predict.filter = filter_bound)
)
lr_bound$estimate(d0)
lr_bound$predict(data.frame(x = 1e6))
```

     1
    10 

## Defining new learners

Statistical or machine learning models for which no constructors are
provided can be implemented with a few lines of code. In what follows we
cover three general cases where the input to the fit function differs.

### Fit function with formula and data arguments

The first general case covers fit functions which expect a formula and
data argument. Both arguments are used then by the fitting routine to
construct the design matrix and response vector. Statistical models are
usually implement with such an interface, with examples including
[`stats::glm`](https://rdrr.io/r/stats/glm.html),
[`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) and
[`earth::earth`](https://rdrr.io/pkg/earth/man/earth.html). The
`learner` R6 class supports these fitting routines by checking if the
provided estimate function has a `formula` and `data` argument. If it
does, then the `formula` and `data` argument are passed on to the
estimate function without further modifications. This is exemplified in
the following for [`stats::glm`](https://rdrr.io/r/stats/glm.html),
where we define a new constructor to return a `learner` class object
that fits a generalized model

``` r

new_glm <- function(formula, ...) {
  learner$new(
    formula = formula,
    estimate = stats::glm,
    predict = stats::predict,
    predict.args = list(type = "response"),
    estimate.args = list(...),
    info = "new glm learner" # optional
  )
}
lr <- new_glm(y ~ age, family = "binomial")
lr
```

    ────────── learner object ──────────
    new glm learner

    Estimate arguments: family=binomial
    Predict arguments: type=response
    Formula: y ~ age 

It can be seen that the optional arguments of `new_glm` define the
`estimate.args` of a constructed learner object. When estimating the
model with

``` r

lr$estimate(pbc)
```

the parameters defined in `estimate.args` are passed on with the
`formula` and `data` to [`stats::glm`](https://rdrr.io/r/stats/glm.html)
(i.e. the function specified via the `estimate` argument). Thus, the
above is equivalent to

``` r

fit <- glm(y ~ age, family = "binomial", data = pbc)
all(coef(fit) == coef(lr$fit))
```

    [1] TRUE

The code further instructs to construct a learner object that uses
[`stats::predict`](https://rdrr.io/r/stats/predict.html) to make
predictions. The learner object similarly passes on the `predict.args`
to [`stats::predict`](https://rdrr.io/r/stats/predict.html) for

``` r

lr$predict(head(pbc))
```

             1          2          3          4          5          6
    0.16171479 0.14624529 0.25614862 0.13566571 0.06272415 0.22071201 

which is equivalent to

``` r

predict(fit, newdata = head(pbc), type = "response")
```

             1          2          3          4          5          6
    0.16171479 0.14624529 0.25614862 0.13566571 0.06272415 0.22071201 

Indeed, the documentation of [`?learner`](../reference/learner.md)
reveals that the predict method always requires an `object` and
`newdata` argument. An important implementation detail is that the
`learner` R6 class allows to overrule the defined `estimate.args` and
`predict.args` in the method calls.

``` r

lr$estimate(pbc, family = "poisson")
lr$predict(head(pbc), type = "link")
```

            1         2         3         4         5         6
    -1.837279 -1.939001 -1.341276 -2.013822 -2.743535 -1.508572 

### Fit function with x and y arguments

Most fitting routines for machine learning models expect a design matrix
and response vector as inputs. The R6 `learner` class supports these
fitting functions by internally processing the `formula` argument via
the [`targeted::design`](../reference/design.md) function. Take the
example of
[`grf::probability_forest`](https://rdrr.io/pkg/grf/man/probability_forest.html),
which expects a `X` (design matrix) and `Y` (response vector) as inputs.

``` r

new_grf <- function(formula, ...) {
  learner$new(
    formula = formula,
    estimate = function(x, y, ...) grf::probability_forest(X = x, Y = y, ...),
    predict = function(object, newdata) {
      predict(object, newdata = newdata)$predictions
    },
    estimate.args = list(...),
    info = "grf::probability_forest"
  )
}
lr <- new_grf(as.factor(y) ~ age + bili, num.trees = 100)
lr$estimate(pbc)
```

Compared to the previous case, the `pbc` data object is not directly
passed on to the fit function.
Instead,[`targeted::design`](../reference/design.md) constructs the
design matrix and response vector from the defined `formula` argument.
As shown previously,

``` r

dsgn <- lr$design(pbc)
```

can be used to inspect the design object. The `x` and `y` attributes of
the returned `design` object are then passed on to the fit function.

### Fit function with single data argument

To support ensemble/meta learners, it is also possible to construct a
learner without providing a formula argument.

``` r

new_sl <- function(learners, ...) {
  learner$new(
    info = "new superlearner",
    estimate = superlearner,
    predict = targeted:::predict.superlearner,
    estimate.args = c(list(learners = learners), list(...))
  )
}
lrs <- list(
  glm = learner_glm(y ~ age, family = "binomial"),
  gam = learner_gam(y ~ s(age), family = "binomial")
)
lr <- new_sl(lrs, nfolds = 2)
lr$estimate(pbc)
lr
```

    ────────── learner object ──────────
    new superlearner

    Estimate arguments: learners=<list>, nfolds=2
    Predict arguments:
    Formula: NULL
    ─────────────────────────────────────
            score weight
    glm 0.1125861      1
    gam 0.1167470      0

In this case, all arguments provided to `lr$estimate` are joined
together with the specified `estimate.args` and passed on to the defined
estimate function (i.e. `superlearner`).

### Specials in formula

Certain learner functions allow for specifying special terms in the
formula. For example, let’s consider an aggregated dataset that includes
only the response variable, treatment, and sex:

``` r

library("data.table")
dd <- data.table(pbc)[!is.na(trt), .(.N), by=.(y,trt,sex)]
print(dd)
```

           y   trt    sex     N
       <int> <int> <fctr> <int>
    1:     1     1      f    13
    2:     0     1      f   124
    3:     0     1      m    19
    4:     0     2      f   123
    5:     1     2      f    16
    6:     0     2      m    12
    7:     1     2      m     3
    8:     1     1      m     2

Next, we fit a Naive Bayes classifier using the `weights` special term
to specify the frequency weights for the estimation

``` r

lr <- learner_naivebayes(y ~ trt + sex + weights(N))
lr$estimate(dd)
lr$predict(dd)
```

    [1] 0.09138473 0.09138473 0.12139346 0.11913520 0.11913520 0.15668515 0.15668515
    [8] 0.12139346

Here `weights.numeric` is simply the identity function

``` r

targeted:::weights.numeric
```

    function(object, ...) object
    <bytecode: 0x5652e5940ed0>
    <environment: namespace:targeted>

To illustrate how to define a custom learner that utilizes special
terms, consider the following example where we introduce a `strata`
term. In this case, we introduce an estimation method that fits a linear
regression model for each value of the strata variable, as well as a
corresponding prediction method

``` r

est <- function(formula, data, strata, ...)
  lapply(levels(strata), \(s) lm(formula, data[which(strata==s),]))

pred <- function(object, newdata, strata, ...) {
  res <- numeric(length(strata))
  for (i in seq_along(levels(strata))) {
    idx <- which(strata == levels(strata)[i])
    res[idx] <- predict(object[[i]], newdata[idx, ], ...)
  }
  return(res)
}
```

The new learner is now defined by including the argument \`specials =
“strata”, which ensures that the strata variable is correctly passed to
both the estimate and predict functions

``` r

lr <- learner$new(y ~ sex + strata(trt),
                  estimate=est, predict=pred, specials = "strata")
des <- lr$design(head(pbc))
des
```

    ────────── design object ──────────

    response (length: 6)

    1   1
    2   0
    ---
    5   0
    6   0

    specials
     - strata [factor]

    design matrix (dim: 6, 1)
        sexf
    1   1
    2   1
    ---
    5   1
    6   1   

``` r

des$strata
```

        1     2     3     4     5     6
    trt=1 trt=1 trt=1 trt=1 trt=2 trt=2
    Levels: trt=1 trt=2

``` r

lr$estimate(pbc)
lr
```

    ────────── learner object ──────────
    Estimate arguments:
    Predict arguments:
    Formula: y ~ sex + strata(trt)
    ─────────────────────────────────────
    [[1]]

    Call:
    lm(formula = formula, data = data[which(strata == s), ])

    Coefficients:
    (Intercept)         sexf
      0.0952381   -0.0003476


    [[2]]

    Call:
    lm(formula = formula, data = data[which(strata == s), ])

    Coefficients:
    (Intercept)         sexf
        0.20000     -0.08489  

``` r

lr$predict(head(pbc))
```

    [1] 0.09489051 0.09489051 0.09523810 0.09489051 0.11510791 0.11510791
