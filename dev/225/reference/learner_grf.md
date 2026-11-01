# Construct a learner

Constructs a [learner](learner.md) class object for fitting generalized
random forest models with
[grf::regression_forest](https://rdrr.io/pkg/grf/man/regression_forest.html)
or
[grf::probability_forest](https://rdrr.io/pkg/grf/man/probability_forest.html).
As shown in the examples, the constructed learner returns predicted
class probabilities of class 2 in case of binary classification. A
`n times p` matrix, with `n` being the number of observations and `p`
the number of classes, is returned for multi-class classification.

## Usage

``` r
learner_grf(
  formula,
  num.trees = 2000,
  min.node.size = 5,
  alpha = 0.05,
  sample.fraction = 0.5,
  num.threads = 1,
  model = "grf::regression_forest",
  info = model,
  learner.args = NULL,
  ...
)
```

## Arguments

- formula:

  (formula) Formula specifying response and design matrix.

- num.trees:

  Number of trees grown in the forest. Note: Getting accurate confidence
  intervals generally requires more trees than getting accurate
  predictions. Default is 2000.

- min.node.size:

  A target for the minimum number of observations in each tree leaf.
  Note that nodes with size smaller than min.node.size can occur, as in
  the original randomForest package. Default is 5.

- alpha:

  A tuning parameter that controls the maximum imbalance of a split.
  Default is 0.05.

- sample.fraction:

  Fraction of the data used to build each tree. Note: If honesty = TRUE,
  these subsamples will further be cut by a factor of honesty.fraction.
  Default is 0.5.

- num.threads:

  Number of threads used in training. By default, the number of threads
  is set to the maximum hardware concurrency.

- model:

  (character) grf model to estimate. Usually regression_forest
  ([grf::regression_forest](https://rdrr.io/pkg/grf/man/regression_forest.html))
  or probability_forest
  ([grf::probability_forest](https://rdrr.io/pkg/grf/man/probability_forest.html)).

- info:

  (character) Optional information to describe the instantiated
  [learner](learner.md) object.

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to `model`

## Value

[learner](learner.md) object.

## Examples

``` r
n <- 5e2
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
lp <- x2*x1 + cos(x1)
yb <- rbinom(n, 1, lava::expit(lp))
y <-  lp + rnorm(n, sd = 0.5**.5)
d <- data.frame(y, yb, x1, x2)

# regression
lr <- learner_grf(y ~ x1 + x2)
lr$estimate(d)
lr$predict(head(d))
#> [1] 1.7288539 3.3169682 1.4148185 0.6533527 0.7314331 1.2631422

# binary classification
lr <- learner_grf(as.factor(yb) ~ x1 + x2, model = "probability_forest")
lr$estimate(d)
lr$predict(head(d)) # predict class probabilities of class 2
#> [1] 0.8816267 0.8536776 0.7823870 0.7034676 0.8120571 0.8149831

# multi-class classification
lr <- learner_grf(Species ~ ., model = "probability_forest")
lr$estimate(iris)
lr$predict(head(iris))
#>         setosa  versicolor    virginica
#> [1,] 0.9992534 0.000125000 0.0006216450
#> [2,] 0.9893655 0.009666468 0.0009680556
#> [3,] 0.9983696 0.001384524 0.0002458333
#> [4,] 0.9950683 0.004685913 0.0002458333
#> [5,] 0.9993534 0.000125000 0.0005216450
#> [6,] 0.9500266 0.047020212 0.0029532277
```
