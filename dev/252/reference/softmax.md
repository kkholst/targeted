# Softmax transformation

Softmax transformation

## Usage

``` r
softmax(x, log = FALSE, ref = TRUE, ...)
```

## Arguments

- x:

  Input matrix (e.g., linear predictors of multinomial logistic model)

- log:

  Return on log-scale (default FALSE)

- ref:

  Add reference level (add 0 column to x)

- ...:

  Additional arguments to lower level functions

## Value

Numeric matrix of dimension n x p, where `n= nrow(x)` and
`p = ncol(x) + (ref==TRUE)`
