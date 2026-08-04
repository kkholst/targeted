# Prediction filter bounding predictions to the observed response range

Generates a prediction filter for the `predict.filter` argument of
[learner](learner.md) that bounds predictions to the range of the
response observed in the estimation data (min(response), max(response)).

## Usage

``` r
predict_filter_bound_dynamic(lower = FALSE, upper = FALSE, response = "y")
```

## Arguments

- lower:

  (logical) If `TRUE`, clamp to the minimum observed response.

- upper:

  (logical) If `TRUE`, clamp to the maximum observed response.

- response:

  (character) Name of the response column in the data.

## Value

A filter generator function (see [learner](learner.md)).

## Author

Benedikt Sommer

## Examples

``` r
data(cars)
lr <- learner_glm(
  speed ~ dist,
  learner.args = list(
    predict.filter = predict_filter_bound_dynamic(
      upper = TRUE, response = "speed"
    )
  )
)
lr$estimate(cars)
lr$predict(data.frame(dist = c(200, 2000)))
#>  1  2 
#> 25 25 
```
