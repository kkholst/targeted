# Predictions for Naive Bayes Classifier

Naive Bayes Classifier predictions

## Usage

``` r
# S3 method for class 'naivebayes'
predict(object, newdata, expectation = NULL, threshold = c(0.001, 0.001), ...)
```

## Arguments

- object:

  density object

- newdata:

  new data on which to make predictions

- expectation:

  Variable to calculate conditional expectation wrt probabilities from
  naivebayes classifier

- threshold:

  Threshold parameters. First element defines the threshold on the
  probabilities and the second element the value to set those truncated
  probabilities to.

- ...:

  Additional arguments to lower level functions

## Author

Klaus K. Holst
