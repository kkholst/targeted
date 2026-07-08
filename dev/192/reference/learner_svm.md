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
#>  1.0757414  0.5556010  2.0339752 -2.2916106  0.6193201  0.4365723 

# binary classification
lr <- learner_svm(as.factor(yb) ~ x1 + x2)
# alternative to transforming response variable to factor
# lr <- learner_svm(yb ~ x1 + x2, type = "C-classification")
lr$estimate(d)
lr$predict(head(d)) # predict class probabilities of class 2
#>          1          2          3          4          5          6 
#> 0.80331322 0.66441900 0.81882867 0.09675849 0.71158292 0.58943184 
lr$predict(head(d), probability = FALSE) # predict labels
#> 1 2 3 4 5 6 
#> 1 1 1 0 1 1 
#> Levels: 0 1

# multi-class classification
lr <- learner_svm(Species ~ .)
lr$estimate(iris)
lr$predict(head(iris))
#>      setosa versicolor   virginica
#> 1 0.9803642 0.01113517 0.008500634
#> 2 0.9730250 0.01782678 0.009148202
#> 3 0.9790748 0.01175510 0.009170099
#> 4 0.9750626 0.01509270 0.009844689
#> 5 0.9795591 0.01147019 0.008970734
#> 6 0.9741740 0.01653949 0.009286489
```
