# Binary regression models with right censored outcomes

Binary regression models with right censored outcomes

## Usage

``` r
riskreg_cens(
  response,
  censoring,
  treatment = NULL,
  prediction = NULL,
  data,
  newdata,
  tau,
  type = "risk",
  M = 1,
  call.response = "phreg",
  args.response = list(),
  call.censoring = "phreg",
  args.censoring = list(),
  preprocess = NULL,
  efficient = TRUE,
  control = list(),
  ...
)
```

## Arguments

- response:

  Response formula (e.g., Surv(time, event) ~ D + W).

- censoring:

  Censoring formula (e.g., Surv(time, event == 0) ~ D + A + W)).

- treatment:

  Optional treatment model (ml_model)

- prediction:

  Optional prediction model (ml_model)

- data:

  data.frame.

- newdata:

  Optional data.frame. In this case the uncentered influence function
  evalued in 'newdata' is returned with nuisance parameters obtained
  from 'data'.

- tau:

  Time-point of interest, see Details.

- type:

  "risk", "treatment", "brier"

- M:

  Number of folds in cross-fitting (M=1 is no cross-fitting).

- call.response:

  Model call for the response model (e.g. "mets::phreg").

- args.response:

  Additional arguments to the response model.

- call.censoring:

  Similar to call.response.

- args.censoring:

  Similar to args.response.

- preprocess:

  (optional) Data pre-processing function.

- efficient:

  If FALSE an IPCW estimator is returned

- control:

  See details

- ...:

  Additional arguments to lower level data pre-processing functions.

## Value

estimate object

## Details

The one-step estimator depends on the calculation of an integral wrt.
the martingale process corresponding to the counting process N(t) =
I(C\>min(T,tau)). This can be decomposed into an integral wrt the
counting process, \\dN_c(t)\\ and the compensator \\d\Lambda_c(t)\\
where the latter term can be computational intensive to calculate.
Rather than calculating this integral in all observed time points, we
can make a coarser evaluation which can be controlled by setting
`control=(sample=N)`. With `N=0` the (computational intensive) standard
evaluation is used.

## Author

Klaus K. Holst, Andreas Nordland
