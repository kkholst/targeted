# R6 class for super learners

Super learner (stacked ensemble) implementation of the
[learner](learner.md) R6 class. `learnerSL$new()` takes the same
arguments as [learner\$new()](learner.md). The base learners are
provided via `estimate.args$learners`, and by default the ensemble is
estimated with [`superlearner()`](superlearner.md). Objects are usually
created with the constructor function [`learner_sl()`](learner_sl.md).
The arguments `formula` and `formula.keep.specials` of `learnerSL$new()`
are not used, and a warning is raised if they are provided.

The formula of the super learner is the formula of its first base
learner. Hence, the methods [`design()`](design.md) and `response()`
return the design and response of the first base learner. The formula is
kept in sync with the first base learner by the
[`update()`](https://rdrr.io/r/stats/update.html) method.

The [`update()`](https://rdrr.io/r/stats/update.html) method changes the
response variable of the super learner and of all base learners. The
covariates of the base learners are never modified by
[`update()`](https://rdrr.io/r/stats/update.html).

The base learners are deep cloned when a `learnerSL` object is created
and when it is cloned with `clone(deep = TRUE)`. Updating a super
learner therefore neither modifies the learner objects used to create it
nor the base learners of any other super learner object.

## See also

[learner_sl](learner_sl.md), [superlearner](superlearner.md),
[cv.learner_sl](cv.learner_sl.md)

## Super class

[`learner`](learner.md) -\> `learner_sl`

## Active bindings

- `learners`:

  Return the list of base learners (read-only). Use learnerSL\$update()
  to update the response variable of the base learners.

## Methods

### Public methods

- [`learner_sl$new()`](#method-learner_sl-initialize)

- [`learner_sl$estimate()`](#method-learner_sl-estimate)

- [`learner_sl$update()`](#method-learner_sl-update)

- [`learner_sl$summary()`](#method-learner_sl-summary)

- [`learner_sl$opt()`](#method-learner_sl-opt)

- [`learner_sl$clone()`](#method-learner_sl-clone)

Inherited methods

- [`learner$design()`](learner.html#method-design)
- [`learner$predict()`](learner.html#method-predict)
- [`learner$print()`](learner.html#method-print)
- [`learner$response()`](learner.html#method-response)

------------------------------------------------------------------------

### `learner_sl$new()`

Create a new super learner object. The arguments are the same as for
[learner\$new()](learner.md).

#### Usage

    learner_sl$new(
      formula = NULL,
      estimate = superlearner,
      predict = predict.superlearner,
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

  (formula) Not used by `learnerSL` objects, because the formula of a
  super learner is the formula of its first base learner. A warning is
  raised if provided. Use learnerSL\$update() to update the response
  variable.

- `estimate`:

  (function) Estimation method of the ensemble. Defaults to
  [superlearner](superlearner.md). A user-defined function must have the
  arguments `data` and `learners` (base learners), e.g.
  `function(data, learners, ...)`.

- `predict`:

  (function) Prediction method. Defaults to
  [predict.superlearner](predict.superlearner.md), which matches the
  default `estimate` method. A user-defined `estimate` method that
  returns a different model object requires a matching `predict` method.

- `predict.args`:

  optional arguments to prediction function

- `estimate.args`:

  (list) Arguments to the `estimate` method. Must contain the element
  `learners`, a list of [learner](learner.md) objects that define the
  ensemble. All base learners must be defined with a formula and share
  the same response variable. The remaining elements (e.g., `nfolds`,
  `meta.learner`, `model.score`) are passed on to the `estimate` method.
  Hence, the defaults of [superlearner](superlearner.md) apply to
  arguments that are not provided.

- `info`:

  (character) Optional description of the model. Defaults to a listing
  of the names of the base learners.

- `specials`:

  optional specials terms (weights, offset, id, subset, ...) passed on
  to [design](design.md)

- `formula.keep.specials`:

  (logical) Not used by `learnerSL` objects. A warning is raised if
  TRUE.

- `predict.filter`:

  function to post-process predictions. Useful to bound predictions or
  handle NAs. The argument is experimental and its behavior may change
  in the future.

- `intercept`:

  (logical) include intercept in design matrix

------------------------------------------------------------------------

### `learner_sl$estimate()`

Estimation method. Estimates the super learner with the base learners of
the object and the `estimate` function (by default
[superlearner](superlearner.md)).

#### Usage

    learner_sl$estimate(data, ..., store = TRUE)

#### Arguments

- `data`:

  (data.frame) Data used to estimate the super learner.

- `...`:

  Additional arguments to the `estimate` function (e.g., `nfolds`),
  which take precedence over `estimate.args`. The base learners cannot
  be changed, and an error is raised if `learners` is provided. Create a
  new super learner with [`learner_sl()`](learner_sl.md) to use other
  base learners.

- `store`:

  (logical) If TRUE, the estimated model is stored inside the object.

------------------------------------------------------------------------

### `learner_sl$update()`

Update the response variable of the super learner and all its base
learners. Each base learner keeps its covariates. A warning is raised
when `formula` specifies covariates.

#### Usage

    learner_sl$update(formula)

#### Arguments

- `formula`:

  (formula or character) Formula or name of the new response variable
  (e.g., `"z"`, `"I(a == 1)"` or `z ~ .`). TODO: change to only
  character because y ~ . is too ambiguous

------------------------------------------------------------------------

### `learner_sl$summary()`

Summary method. See [learner\$summary()](learner.md).

#### Usage

    learner_sl$summary()

------------------------------------------------------------------------

### `learner_sl$opt()`

Get options

#### Usage

    learner_sl$opt(arg)

#### Arguments

- `arg`:

  (character) Name of option to get value of. Use `"learners"` to get
  the base learners.

------------------------------------------------------------------------

### `learner_sl$clone()`

The objects of this class are cloneable with this method.

#### Usage

    learner_sl$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
n <- 200
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d$y <- d$x1 + rnorm(n)
d$z <- d$x1 - d$x2 + rnorm(n)

lrs <- list(
  "mean" = learner_glm(y ~ 1),
  "glm" = learner_glm(y ~ x1 + x2)
)
sl <- learnerSL$new(estimate.args = list(learners = lrs, nfolds = 2))
sl$formula # formula of the first base learner
#> y ~ 1
#> <environment: 0x55e9673c7488>

# update the response variable of the super learner and all base learners
sl$update("z")
sl$formula
#> z ~ 1
#> <environment: 0x55e9673c7488>
lapply(sl$learners, \(lr) lr$formula)
#> $mean
#> z ~ 1
#> <environment: 0x55e9673c7488>
#> 
#> $glm
#> z ~ x1 + x2
#> <environment: 0x55e9673c7488>
#> 

sl$estimate(d)
sl$predict(head(d))
#> [1] -0.6737501 -0.8923655 -1.8966800 -1.4373709  2.2343352 -1.2449426
```
