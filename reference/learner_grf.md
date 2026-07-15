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
#> [1]  0.7517240  0.8353015 -2.6525150  1.0890892  1.9278862  0.8442065

# binary classification
lr <- learner_grf(as.factor(yb) ~ x1 + x2, model = "probability_forest")
lr$estimate(d)
lr$predict(head(d)) # predict class probabilities of class 2
#> [1] 0.6426667 0.7047402 0.1808844 0.6902865 0.6723559 0.6877260

# multi-class classification
lr <- learner_grf(Species ~ ., model = "probability_forest")
lr$estimate(iris)
lr$predict(head(iris))
#>         setosa   versicolor    virginica
#> [1,] 0.9990190 0.0004005952 0.0005803571
#> [2,] 0.9861171 0.0130947691 0.0007880952
#> [3,] 0.9987476 0.0011523810 0.0001000000
#> [4,] 0.9961931 0.0036355159 0.0001714286
#> [5,] 0.9987155 0.0003708333 0.0009136905
#> [6,] 0.9493131 0.0459268589 0.0047600395
```
