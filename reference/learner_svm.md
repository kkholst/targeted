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
#>  1.9355904 -0.9661012  2.5837078  2.4158272  0.3293733  2.8257696 

# binary classification
lr <- learner_svm(as.factor(yb) ~ x1 + x2)
# alternative to transforming response variable to factor
# lr <- learner_svm(yb ~ x1 + x2, type = "C-classification")
lr$estimate(d)
lr$predict(head(d)) # predict class probabilities of class 2
#>         1         2         3         4         5         6 
#> 0.1732894 0.7955262 0.1990194 0.1346051 0.3312072 0.1188097 
lr$predict(head(d), probability = FALSE) # predict labels
#> 1 2 3 4 5 6 
#> 1 0 1 1 1 1 
#> Levels: 0 1

# multi-class classification
lr <- learner_svm(Species ~ .)
lr$estimate(iris)
lr$predict(head(iris))
#>      setosa versicolor   virginica
#> 1 0.9813732 0.01029040 0.008336362
#> 2 0.9744466 0.01655585 0.008997531
#> 3 0.9801363 0.01087089 0.008992805
#> 4 0.9763435 0.01399389 0.009662596
#> 5 0.9806186 0.01060540 0.008776005
#> 6 0.9755778 0.01535009 0.009072088
```
