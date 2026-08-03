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

  additional arguments (e.g, specials such weights, offsets, ...)

- intercept:

  (logical) If FALSE an intercept is not included in the design matrix

- response:

  (logical) if FALSE the response variable is dropped

- rm.envir:

  (logical) Remove environment from terms attribute of returned object

- specials:

  character vector specifying functions in the formula that should be
  marked as special in the [terms](https://rdrr.io/r/stats/terms.html)
  object

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

## Author

Klaus Kähler Holst
