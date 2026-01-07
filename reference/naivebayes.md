# Naive Bayes classifier

Naive Bayes Classifier

## Usage

``` r
naivebayes(
  formula,
  data,
  weights = NULL,
  kernel = FALSE,
  laplace.smooth = 0,
  prior = NULL,
  ...
)
```

## Arguments

- formula:

  Formula with syntax: response ~ predictors \| weights

- data:

  data.frame

- weights:

  optional frequency weights

- kernel:

  If TRUE a kernel estimator is used for numeric predictors (otherwise a
  gaussian model is used)

- laplace.smooth:

  Laplace smoothing

- prior:

  optional prior probabilities (default estimated from data)

- ...:

  additional arguments to lower level functions

## Value

An object of class '`naivebayes`' is returned. See
[`naivebayes-class`](naivebayes-class.md) for more details about this
class and its generic functions.

## Author

Klaus K. Holst

## Examples

``` r
library(data.table)
data(iris)
m <- naivebayes(Species ~ Sepal.Width + Petal.Length, data = iris)
pr <- predict(m, newdata = iris)

# using weights to reduce the size of the dataset
n <- 5e2
x <- rnorm(n, sd = 2) > 0
y <- rbinom(n, 1, lava::expit(x))
# full data set
d1 <- data.frame(y, x = as.factor(x > 0))
m1 <- naivebayes(y ~ x, data = d1)
# reduced data set
d2 <- data.table(d1)[, .(.N), by = .(y, x)]
m2 <- naivebayes(y ~ x, data = d2, weights = d2$N)
all(predict(m1, d1) == predict(m2, d1))
#> [1] TRUE
```
