# R6 class for super learners

Super learner (stacked ensemble) implementation of the
[learner](learner.md) R6 class. Objects are usually created with
[`learner_sl()`](learner_sl.md). A `learnerSL` object owns a list of
base learners, which are estimated and combined by
[`superlearner()`](superlearner.md).

The formula of the super learner is derived from its base learners. It
is defined by their common response variable and the union of the
covariates of all base learners. Special terms (e.g., weights or
offsets) of the base learners are currently not handled and appear as
covariates.

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

Create a new super learner object

#### Usage

    learner_sl$new(
      learners,
      info = NULL,
      nfolds = 5L,
      meta.learner = metalearner_nnls,
      model.score = mse,
      learner.args = NULL,
      ...
    )

#### Arguments

- `learners`:

  (list) List of [learner](learner.md) objects that define the ensemble.
  All learners must be defined with a formula and share the same
  response variable.

- `info`:

  (character) Optional information to describe the instantiated object.
  Defaults to a listing of the names of `learners`.

- `nfolds`:

  (integer) Number of folds to use in cross-validation to estimate the
  ensemble weights.

- `meta.learner`:

  (function) Algorithm to learn the ensemble weights (see
  [superlearner](superlearner.md)).

- `model.score`:

  (function) Model scoring method (see [superlearner](superlearner.md)).

- `learner.args`:

  (list) Additional arguments to [learner\$new()](learner.md).

- `...`:

  Additional arguments to [superlearner](superlearner.md).

------------------------------------------------------------------------

### `learner_sl$estimate()`

Estimation method. Estimates the super learner with
[superlearner](superlearner.md).

#### Usage

    learner_sl$estimate(data, ..., learners = private$.learners, store = TRUE)

#### Arguments

- `data`:

  (data.frame) Data used to estimate the super learner.

- `...`:

  Additional arguments to [superlearner](superlearner.md) and the
  prediction filter generator function.

- `learners`:

  (list) Base learners. Defaults to the base learners of the object.

- `store`:

  (logical) If TRUE, the estimated model is stored inside the object.

------------------------------------------------------------------------

### `learner_sl$update()`

Update the response variable of the super learner and all its base
learners. Each base learner keeps its covariates. A warning is raised
when `formula` specifies covariates which differ from the covariates of
the super learner, because the covariates are not updated.

#### Usage

    learner_sl$update(formula)

#### Arguments

- `formula`:

  (formula or character) Formula or name of the new response variable
  (e.g., `"z"`, `"I(a == 1)"` or `z ~ .`).

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

sl <- learnerSL$new(list(
  "mean" = learner_glm(y ~ 1),
  "glm" = learner_glm(y ~ x1 + x2)
), nfolds = 2)
sl$formula # common response and union of covariates
#> y ~ x1 + x2
#> <environment: 0x560b5dfd1680>

# update the response variable of the super learner and all base learners
sl$update("z")
sl$formula
#> z ~ x1 + x2
#> <environment: 0x560b5dfd1680>
lapply(sl$learners, \(lr) lr$formula)
#> $mean
#> z ~ 1
#> <environment: 0x560b5e22dc50>
#> 
#> $glm
#> z ~ x1 + x2
#> <environment: 0x560b5e236c60>
#> 

sl$estimate(d)
sl$predict(head(d))
#> [1] -0.6737501 -0.8923655 -1.8966800 -1.4373709  2.2343352 -1.2449426
```
