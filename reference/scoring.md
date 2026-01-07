# Predictive model scoring

Predictive model scoring

## Usage

``` r
scoring(
  response,
  ...,
  type = "quantitative",
  levels = NULL,
  metrics = NULL,
  weights = NULL,
  names = NULL,
  object = NULL,
  newdata = NULL,
  messages = 1
)
```

## Arguments

- response:

  Observed response

- ...:

  model predictions (continuous predictions or class probabilities
  (matrices))

- type:

  continuous or categorical response (the latter is automatically chosen
  if response is a factor, otherwise a continuous response is assumed)

- levels:

  (optional) unique levels in response variable

- metrics:

  which metrics to report

- weights:

  optional frequency weights

- names:

  (optional) character vector of the model names in the output. If
  omitted these will be taken from the names of the ellipsis argument
  (...)

- object:

  optional model object

- newdata:

  (optional) data.frame on which to evaluate the model performance

- messages:

  controls amount of messages/warnings (0: none)

## Value

Numeric matrix of dimension m x p, where m is the number of different
models and p is the number of model metrics

## Examples

``` r
data(iris)
set.seed(1)
dat <- lava::csplit(iris,2)
g1 <- naivebayes(Species ~ Sepal.Width + Petal.Length, data=dat[[1]])
g2 <- naivebayes(Species ~ Sepal.Width, data=dat[[1]])
pr1 <- predict(g1, newdata=dat[[2]], wide=TRUE)
pr2 <- predict(g2, newdata=dat[[2]], wide=TRUE)
table(colnames(pr1)[apply(pr1,1,which.max)], dat[[2]]$Species)
#>             
#>              setosa versicolor virginica
#>   setosa         22          0         0
#>   versicolor      0         25         3
#>   virginica       0          2        23
table(colnames(pr2)[apply(pr2,1,which.max)], dat[[2]]$Species)
#>             
#>              setosa versicolor virginica
#>   setosa         17          1         8
#>   versicolor      1         15        10
#>   virginica       4         11         8
scoring(dat[[2]]$Species, pr1=pr1, pr2=pr2)
#>          brier -logscore
#> pr1 0.08719281 0.1419163
#> pr2 0.51025054 0.8547227
## quantitative response:
scoring(response=1:10, prediction=rnorm(1:10))
#>                 mse      mae
#> prediction 42.27782 5.757058
```
