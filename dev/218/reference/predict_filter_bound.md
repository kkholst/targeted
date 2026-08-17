# Prediction filter bounding predictions to fixed range

Generates a prediction filter for the `predict.filter` argument of
[learner](learner.md) that bounds predictions to a fixed range (lower,
upper).

## Usage

``` r
predict_filter_bound(lower = NULL, upper = NULL)
```

## Arguments

- lower:

  (numeric) Lower bound, or `NULL` for no lower bound.

- upper:

  (numeric) Upper bound, or `NULL` for no upper bound.

## Value

A filter generator function (see [learner](learner.md)).

## Author

Benedikt Sommer

## Examples

``` r
data(cars)
lr <- learner_glm(
  speed ~ dist,
  learner.args = list(predict.filter = predict_filter_bound(upper = 10))
)
lr$estimate(cars)
lr$predict(data.frame(dist = c(10, 50)))
#>         1         2 
#>  9.939581 10.000000 
```
