# Construct a learner

Constructs a [learner](learner.md) class object for fitting a naive
bayes classifier with [naivebayes](naivebayes.md). As shown in the
examples, the constructed learner returns predicted class probabilities
of class 2 in case of binary classification. A `n times p` matrix, with
`n` being the number of observations and `p` the number of classes, is
returned for multi-class classification.

## Usage

``` r
learner_naivebayes(
  formula,
  info = "Naive Bayes",
  laplace.smooth = 0,
  kernel = FALSE,
  learner.args = NULL,
  ...
)
```

## Arguments

- formula:

  (formula) Formula specifying response and design matrix.

- info:

  (character) Optional information to describe the instantiated
  [learner](learner.md) object.

- laplace.smooth:

  Laplace smoothing

- kernel:

  If TRUE a kernel estimator is used for numeric predictors (otherwise a
  gaussian model is used)

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to [naivebayes](naivebayes.md).

## Value

[learner](learner.md) object.

## Examples

``` r
n <- 5e2
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
y <- rbinom(n, 1, lava::expit(x2*x1 + cos(x1)))
d <- data.frame(y, x1, x2)

# binary classification
lr <- learner_naivebayes(y ~ x1 + x2)
lr$estimate(d)
lr$predict(head(d))
#> [1] 0.5037489 0.5588338 0.5754089 0.5635060 0.4972883 0.5299070

# multi-class classification
lr <- learner_naivebayes(Species ~ .)
lr$estimate(iris)
lr$predict(head(iris))
#>      setosa   versicolor    virginica
#> [1,]      1 1.357840e-18 7.112825e-26
#> [2,]      1 1.514805e-17 2.348197e-25
#> [3,]      1 1.073040e-18 2.340265e-26
#> [4,]      1 1.466193e-17 2.954923e-25
#> [5,]      1 4.532911e-19 2.883896e-26
#> [6,]      1 1.490941e-14 1.757519e-21
```
