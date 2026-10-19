# Predict the cumulative hazard/survival function for a survival model

Predict the cumulative hazard/survival function for a survival model

## Usage

``` r
cumhaz(
  object,
  newdata,
  times = NULL,
  individual.time = FALSE,
  extend = FALSE,
  ...
)
```

## Arguments

- object:

  Survival model object: phreg, coxph, rfsrc, ranger, survSuperLearner

- newdata:

  data.frame

- times:

  numeric vector: Time points at which the survival model is evaluated.
  If NULL, the time points associated with the survival model is used.

- individual.time:

  logical: If TRUE the survival object is evaluated at different time
  points for each row in newdata. The number of rows in newdata and the
  length of times must be the same.

- extend:

  if TRUE, prints information for all specified 'times’, even if there
  are no subjects left at the end of the specified ‘times’ (see
  [survival::summary.survfit](https://rdrr.io/pkg/survival/man/summary.survfit.html)).

- ...:

  Additional arguments.

## Value

List with elements:

- time: numeric vector

- chf: cumulative hazard function. If individual.time = FALSE, matrix
  with dimension (nrow(newdata), length(times)). If individual.time =
  TRUE, vector of length length(times).

- surv: survival function, exp(-chf).

- dchf: t(diff(rbind(0, t(chf))))

## Author

Klaus K. Holst, Andreas Nordland
