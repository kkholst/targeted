# Non-negative least squares meta learner

Estimates the ensemble weights of a [superlearner](superlearner.md) by
minimizing the cross-validated MSE via non-negative least squares
regression. The estimated weights are non-negative and normalized to sum
to one.

## Usage

``` r
metalearner_nnls(y, pred, method = "quadprog", ...)
```

## Arguments

- y:

  (numeric) Response vector.

- pred:

  (matrix) Matrix of cross-validated predictions with one column per
  candidate learner.

- method:

  (character) Quadratic-programming solver used to compute the
  non-negative least squares weights. Either `"quadprog"` (default,
  using
  [quadprog::solve.QP](https://rdrr.io/pkg/quadprog/man/solve.QP.html))
  or `"nnls"` (using
  [nnls::nnls](https://rdrr.io/pkg/nnls/man/nnls.html)).

- ...:

  Additional arguments (currently ignored).

## Value

(numeric) Vector of ensemble weights, one element per column of `pred`.

## Details

`targeted:::metalearner_nnls2` is an internal wrapper for using the
`"nnls"` package instead of `"quadprog"`.

## See also

[superlearner](superlearner.md) [learner_sl](learner_sl.md)
