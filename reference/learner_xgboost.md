# Construct a learner

Constructs a [learner](learner.md) class object for
[xgboost::xgboost](https://rdrr.io/pkg/xgboost/man/xgboost.html).

## Usage

``` r
learner_xgboost(
  formula,
  max_depth = 2L,
  learning_rate = 1,
  nrounds = 2L,
  subsample = 1,
  reg_lambda = 1,
  objective = "reg:squarederror",
  info = paste("xgboost", objective),
  learner.args = NULL,
  ...
)
```

## Arguments

- formula:

  (formula) Formula specifying response and design matrix.

- max_depth:

  (integer) Maximum depth of a tree.

- learning_rate:

  (numeric) Learning rate.

- nrounds:

  Number of boosting iterations / rounds.

  Note that the number of default boosting rounds here is not
  automatically tuned, and different problems will have vastly different
  optimal numbers of boosting rounds.

- subsample:

  (numeric) Subsample ratio of the training instance.

- reg_lambda:

  (numeric) L2 regularization term on weights.

- objective:

  (character) Specify the learning task and the corresponding learning
  objective. See
  [xgboost::xgboost](https://rdrr.io/pkg/xgboost/man/xgboost.html) for
  all available options.

- info:

  (character) Optional information to describe the instantiated
  [learner](learner.md) object.

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to
  [xgboost::xgboost](https://rdrr.io/pkg/xgboost/man/xgboost.html).

## Value

[learner](learner.md) object.

## Examples

``` r
n  <- 1e3
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
lp <- x2*x1 + cos(x1)
yb <- rbinom(n, 1, lava::expit(lp))
y <-  lp + rnorm(n, sd = 0.5**.5)
d0 <- data.frame(y, yb, x1, x2)

# regression
lr <- learner_xgboost(y ~ x1 + x2, nrounds = 5)
lr$estimate(d0)
lr$predict(head(d0))
#> [1] -1.129594564 -0.008856855 -1.472966552  2.784336090  1.873351097
#> [6]  1.034347534

# binary classification
lr <- learner_xgboost(yb ~ x1 + x2, nrounds = 5,
 objective = "binary:logistic"
)
lr$estimate(d0)
lr$predict(head(d0))
#> [1] 0.1595791 0.5941457 0.1956527 0.9064949 0.7835268 0.7215205

# multi-class classification
d0 <- iris
d0$y <- as.numeric(d0$Species)- 1

lr <- learner_xgboost(y ~ ., objective = "multi:softprob", num_class = 3)
lr$estimate(d0)
lr$predict(head(d0))
#>           [,1]       [,2]       [,3]
#> [1,] 0.9290679 0.03546607 0.03546607
#> [2,] 0.9290679 0.03546607 0.03546607
#> [3,] 0.9290679 0.03546607 0.03546607
#> [4,] 0.9290679 0.03546607 0.03546607
#> [5,] 0.9290679 0.03546607 0.03546607
#> [6,] 0.9290679 0.03546607 0.03546607
```
