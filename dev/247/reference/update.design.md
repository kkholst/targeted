# Update method for design

Update method for design

## Usage

``` r
# S3 method for class 'design'
update(object, data = NULL, response = FALSE, levels, ...)
```

## Examples

``` r
n <- 1e2
d0 <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
d0$w <- runif(n)
dnew <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
```
