# targeted class object

The functions [`riskreg`](riskreg.md) and [`ate`](ate.md) returns an
object of the type `targeted`.

An object of class '`targeted`' is a list with at least the following
components:

- estimate:

  An `estimate` object with the target parameter estimates (see
  [`estimate.default`](http://kkholst.github.io/lava/reference/estimate.default.md))

- opt:

  Object returned from the applied optimization routine

- npar:

  number of parameters of the model (target and nuisance)

- type:

  String describing the model

## Value

objects of the S3 class '`targeted`'

## S3 generics

The following S3 generic functions are available for an object of class
`targeted`:

- `coef`:

  Extract target coefficients of the estimated model.

- `vcov`:

  Extract the variance-covariance matrix of the target parameters.

- `IC`:

  Extract the estimated influence function.

- `print`:

  Print estimates of the target parameters.

- `summary`:

  Extract information on both target parameeters and estimated nuisance
  model.

## See also

[`riskreg`](riskreg.md), [`ate`](ate.md)

## Examples

``` r
## See example(riskreg) for examples
```
