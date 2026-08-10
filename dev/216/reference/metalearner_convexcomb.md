# Convex combination meta learner

Estimates the ensemble weights of a [superlearner](superlearner.md) by
minimizing the cross-validated MSE as a convex combination of the
candidate predictions, i.e. by least squares regression of the response
on the candidate predictions subject to the constraint that the weights
are non-negative and sum to one.

## Usage

``` r
metalearner_convexcomb(y, pred, ...)
```

## Arguments

- y:

  (numeric) Response vector.

- pred:

  (matrix) Matrix of cross-validated predictions with one column per
  candidate learner.

- ...:

  Additional arguments (currently ignored).

## Value

(numeric) Vector of ensemble weights, one element per column of `pred`.

## See also

[superlearner](superlearner.md) [learner_sl](learner_sl.md)
