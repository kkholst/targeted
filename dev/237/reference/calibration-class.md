# calibration class object

The functions [`calibration`](calibration.md) returns an object of the
class `calibration`.

An object of class '`calibration`' is a list with at least the following
components:

- stepfun:

  estimated step-functions (see `stepfun`) for each class

- classes:

  the unique classes

- model:

  model/method type (string)

- xy:

  list of data.frame's with predictions (pr) and estimated probabilities
  of success (only for 'bin' method)

## Value

objects of the S3 class '`calibration`'

## S3 generics

The following S3 generic functions are available for an object of class
`calibration`:

- `predict`:

  Apply calibration to new data.

- `plot`:

  Plot the calibration curves (reliability plot).

- `print`:

  Basic print method.

## See also

[`calibration`](calibration.md), [`calibrate`](calibration.md)

## Examples

``` r
## See example(calibration) for examples
```
