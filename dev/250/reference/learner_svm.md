# Construct a learner

Constructs a [learner](learner.md) class object for fitting support
vector machines with
[e1071::svm](https://rdrr.io/pkg/e1071/man/svm.html). As shown in the
examples, the constructed learner returns predicted class probabilities
of class 2 in case of binary classification. A `n times p` matrix, with
`n` being the number of observations and `p` the number of classes, is
returned for multi-class classification.

## Usage

``` r
learner_svm(
  formula,
  info = "e1071::svm",
  cost = 1,
  epsilon = 0.1,
  kernel = "radial",
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

- cost:

  cost of constraints violation (default: 1)—it is the ‘C’-constant of
  the regularization term in the Lagrange formulation.

- epsilon:

  epsilon in the insensitive-loss function (default: 0.1)

- kernel:

  the kernel used in training and predicting. You might consider
  changing some of the following parameters, depending on the kernel
  type.  

  linear:

  :   \\u'v\\

  polynomial:

  :   \\(\gamma u'v + coef0)^{degree}\\

  radial basis:

  :   \\e^(-\gamma \|u-v\|^2)\\

  sigmoid:

  :   \\tanh(\gamma u'v + coef0)\\

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to
  [e1071::svm](https://rdrr.io/pkg/e1071/man/svm.html).

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
lr <- learner_svm(y ~ x1 + x2)
lr$estimate(d)
lr$predict(head(d))
#>          1          2          3          4          5          6 
#>  3.4811254  0.5558726 -1.4221001 -0.2261048  1.5689569  0.7125958 

# binary classification
lr <- learner_svm(as.factor(yb) ~ x1 + x2)
# alternative to transforming response variable to factor
# lr <- learner_svm(yb ~ x1 + x2, type = "C-classification")
lr$estimate(d)
lr$predict(head(d)) # predict class probabilities of class 2
#>         1         2         3         4         5         6 
#> 0.1846311 0.5695382 0.8927013 0.4565281 0.1526657 0.2675977 
lr$predict(head(d), probability = FALSE) # predict labels
#> 1 2 3 4 5 6 
#> 1 1 0 1 1 1 
#> Levels: 0 1

# multi-class classification
lr <- learner_svm(Species ~ .)
lr$estimate(iris)
lr$predict(head(iris))
#>      setosa versicolor   virginica
#> 1 0.9805323 0.01101799 0.008449738
#> 2 0.9732059 0.01767209 0.009122002
#> 3 0.9792496 0.01163829 0.009112101
#> 4 0.9752558 0.01496091 0.009783259
#> 5 0.9797429 0.01136428 0.008892840
#> 6 0.9743965 0.01642316 0.009180370
```
