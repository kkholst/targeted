# R6 class for prediction models

Replaced by [learner](learner.md)

## Super class

[`targeted::learner`](learner.md) -\> `ml_model`

## Methods

### Public methods

- [`ml_model$new()`](#method-ml_model-new)

- [`ml_model$clone()`](#method-ml_model-clone)

Inherited methods

- [`targeted::learner$design()`](learner.html#method-design)
- [`targeted::learner$estimate()`](learner.html#method-estimate)
- [`targeted::learner$opt()`](learner.html#method-opt)
- [`targeted::learner$predict()`](learner.html#method-predict)
- [`targeted::learner$print()`](learner.html#method-print)
- [`targeted::learner$response()`](learner.html#method-response)
- [`targeted::learner$summary()`](learner.html#method-summary)
- [`targeted::learner$update()`](learner.html#method-update)

------------------------------------------------------------------------

### Method `new()`

Create a new prediction model object

#### Usage

    ml_model$new(...)

#### Arguments

- `...`:

  deprecated

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ml_model$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
