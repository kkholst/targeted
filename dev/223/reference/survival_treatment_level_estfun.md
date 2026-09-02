# Treatment level estimating functions for survival outcomes under right censoring

Treatment level estimating functions for survival outcomes under right
censoring

## Usage

``` r
survival_treatment_level_estfun(
  type = "risk",
  data,
  tau,
  survival_models,
  treatment_model,
  control
)
```

## Arguments

- type:

  Character string, outcome of interest: "risk": P(T \<= tau\|A=a),
  "surv": P(T \> tau\|A=a)

- data:

  data.frame

- tau:

  Numeric, time-point of interest

- survival_models:

  List of survival models, see fit_survival_models()

- treatment_model:

  Treatment model, see fit_treatment_model()

- control:

  List of control parameters, list(sample, blocksize)

## Value

List with matrix elements estfun, or, and ipw.

## Author

Andreas Nordland
