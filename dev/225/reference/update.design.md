# Update method for design

Update method for design

## Usage

``` r
# S3 method for class 'design'
update(object, data = NULL, response = FALSE, levels, ...)
```

## Arguments

- object:

  (design) object returned by [design](design.md)

- data:

  (data.frame) data on which the design is re-evaluated. If `NULL`, the
  empty data.frame stored in `object` is used.

- response:

  (logical) if FALSE the response variable is dropped

- levels:

  (list) named list of character vectors giving the full set of levels
  to be assumed for each factor. Defaults to the levels stored in
  `object`, such that factors are coded as in the original design.

- ...:

  additional arguments (currently ignored)

## Value

An object of class 'design'

## Examples

``` r
n <- 1e2
d0 <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
d0$w <- runif(n)
dnew <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
```
