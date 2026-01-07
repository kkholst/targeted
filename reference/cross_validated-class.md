# cross_validated class object

The functions [`cv`](cv.default.md) returns an object of the type
`cross_validated`.

An object of class '`cross_validated`' is a list with at least the
following components:

- cv:

  An array with the model score(s) evaluated for each fold, repetition,
  and model estimates (see
  [`estimate.default`](http://kkholst.github.io/lava/reference/estimate.default.md))

- names:

  Names (character vector) of the models

- rep:

  number of repetitions of the CV

- folds:

  Number of folds of the CV

## Value

objects of the S3 class '`cross_validated`'

## S3 generics

The following S3 generic functions are available for an object of class
`cross_validated`:

- `coef`:

  Extract average model scores from the cross-validation procedure.

- `print`:

  Basic print method.

- `summary`:

  Summary of the cross-validation procedure.

## See also

[`cv`](cv.default.md)

## Examples

``` r
# See example(cv) for examples
```
