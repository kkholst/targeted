# Discrete meta learner

Implements the discrete super learner: the candidate learner with the
lowest risk (computed via the `model.score` argument of
[superlearner](superlearner.md)) is given weight one and all other
learners weight zero.

## Usage

``` r
metalearner_discrete(y, pred, model.score, ...)
```

## Arguments

- y:

  (numeric) Response vector.

- pred:

  (matrix) Matrix of cross-validated predictions with one column per
  candidate learner.

- model.score:

  (function) Method for scoring the predictions of each base learner.

- ...:

  Additional arguments (currently ignored).

## Value

(numeric) Vector of ensemble weights, one element per column of `pred`.

## See also

[superlearner](superlearner.md) [learner_sl](learner_sl.md)
