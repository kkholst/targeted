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
#>  0.6783454  1.3596236 -0.1030411  0.7134114  1.5191521  1.6972825 

# binary classification
lr <- learner_svm(as.factor(yb) ~ x1 + x2)
# alternative to transforming response variable to factor
# lr <- learner_svm(yb ~ x1 + x2, type = "C-classification")
lr$estimate(d)
lr$predict(head(d)) # predict class probabilities of class 2
#>         1         2         3         4         5         6 
#> 0.6729768 0.7758674 0.4995028 0.7138181 0.7480310 0.7771504 
lr$predict(head(d), probability = FALSE) # predict labels
#> 1 2 3 4 5 6 
#> 1 1 1 1 1 1 
#> Levels: 0 1

# multi-class classification
lr <- learner_svm(Species ~ .)
lr$estimate(iris)
lr$predict(head(iris))
#>      setosa versicolor   virginica
#> 1 0.9810747 0.01047900 0.008446278
#> 2 0.9740656 0.01683061 0.009103743
#> 3 0.9798219 0.01106836 0.009109707
#> 4 0.9759835 0.01423589 0.009780655
#> 5 0.9803026 0.01079865 0.008898780
#> 6 0.9751938 0.01561016 0.009196055
```
