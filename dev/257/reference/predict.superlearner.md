# Predict Method for superlearner Fits

Obtains predictions for ensemble model or individual learners.

## Usage

``` r
# S3 method for class 'superlearner'
predict(object, newdata, all.learners = FALSE, ...)
```

## Arguments

- object:

  (superlearner) Fitted [superlearner](superlearner.md) object.

- newdata:

  (data.frame) Data in which to look for variables with which to
  predict.

- all.learners:

  (logical) If FALSE (default), then return the predictions from the
  ensemble model. Otherwise, return predictions of from all individual
  learners.

- ...:

  Not used.

## Value

numeric (`all.learners = FALSE`) or matrix (`all.learners = TRUE`)
