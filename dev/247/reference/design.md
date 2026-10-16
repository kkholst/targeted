# Extract design matrix

Extract design matrix from data.frame and formula

## Usage

``` r
design(
  formula,
  data,
  ...,
  intercept = FALSE,
  response = TRUE,
  rm.envir = FALSE,
  specials = NULL,
  specials.call = NULL,
  levels = NULL,
  design.matrix = TRUE,
  na.action = na.omit
)
```

## Arguments

- formula:

  formula

- data:

  data.frame

- ...:

  additional named arguments (e.g., specials such as `weights = w` or
  `offset = o`) that are passed on to
  [stats::model.frame](https://rdrr.io/r/stats/model.frame.html) and
  returned as specials. Unnamed arguments and `subset` are not allowed.

- intercept:

  (logical) If FALSE an intercept is not included in the design matrix

- response:

  (logical) if FALSE the response variable is dropped

- rm.envir:

  (logical) Remove environment from terms attribute of returned object

- specials:

  character vector specifying functions in the formula that should be
  marked as special in the [terms](https://rdrr.io/r/stats/terms.html)
  object. Functions are looked up by following the lexical scoping of
  `model.frame` (see details). Specials that are missing in the formula
  will create special fields in the returned design object with their
  value set to `NULL`.

- specials.call:

  (call) specials optionally defined as a call-type

- levels:

  a named list of character vectors giving the full set of levels to be
  assumed for each factor

- design.matrix:

  (logical) if FALSE then only response and specials are returned.
  Otherwise, the design.matrix `x` is als part of the returned object.

- na.action:

  (function) method to handle missing data (default: `na.omit`)

## Value

An object of class 'design'

## See also

[update.design](update.design.md)

## Author

Klaus Kähler Holst

## Examples

``` r
n <- 1e2
d0 <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
d0$w <- runif(n)

dd <- design(y ~ a + x, data = d0)
list(response = head(dd$y, 2), design.matrix = head(dd$x, 2))
#> $response
#>           1           2 
#> -1.28373448 -0.03446884 
#> 
#> $design.matrix
#>   a          x
#> 1 1  1.8687205
#> 2 1 -0.1739599
#> 

# specials
# special variable is extracted and removed from design matrix
dd <- design(y ~ a + x + weights(w), d0, specials = "weights")
list(weights = head(dd$weights, 2), design.matrix = head(dd$x, 2))
#> $weights
#>          1          2 
#> 0.99034101 0.04562852 
#> 
#> $design.matrix
#>   a          x
#> 1 1  1.8687205
#> 2 1 -0.1739599
#> 
# declared special variable is missing in formula. the associated field
# is NULL
dd <- design(y ~ a + x, d0, specials = "weights")
dd$weights
#> NULL
head(dd$x)
#>   a           x
#> 1 1  1.86872053
#> 2 1 -0.17395989
#> 3 0 -1.79603412
#> 4 1  2.39332590
#> 5 1  0.05762734
#> 6 0  0.92038579
```
