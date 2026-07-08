# naivebayes class object

The functions [naivebayes](naivebayes.md) returns an object of the type
`naivebayes`.

An object of class '`naivebayes`' is a list with at least the following
components:

- prior:

  Matrix with prior probabilities, i.e. marginal class probabilities
  Pr(class)

- pcond:

  list of matrices with conditional probabilities of the features given
  the classes (one list element per class), Pr(x\|class)

- classes:

  Names (character vector) of the classes

- xvar:

  Names of predictors

- xmodel:

  Conditional model for each predictor

- design:

  Model design object

- call:

  The function call which instantiated the object

## Value

objects of the S3 class '`naivebayes`'

## S3 generics

The following S3 generic functions are available for an object of class
`naivebayes`:

- `predict`:

  Predict class probabilities for new features data.

- `print`:

  Basic print method.

## See also

[`naivebayes()`](naivebayes.md)

## Examples

``` r
## See example(naivebayes) for examples
```
