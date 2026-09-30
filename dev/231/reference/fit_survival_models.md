# Fit survival nuisance models

Fit survival nuisance models

## Usage

``` r
fit_survival_models(
  data,
  response,
  censoring,
  response_call = "phreg",
  response_args = list(),
  censoring_call = "phreg",
  censoring_args = list()
)
```

## Arguments

- data:

  data.frame

- response:

  Response formula (e.g., Surv(time, event) ~ A + W)

- censoring:

  Censoring formula (e.g., Surv(time, event == 0) ~ A + W))

- response_call:

  Model call for the response model (e.g. "mets::phreg")

- response_args:

  Additional arguments passed to the response model

- censoring_call:

  Similar to response_callb

- censoring_args:

  Similar to response_args

## Value

List with elements T_model and C_model

## Author

Andreas Nordland, Klaus K. Holst
