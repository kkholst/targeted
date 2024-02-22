# targeted 0.5
- `cate` now also returns the expected potential outcomes and influence functions
- Bug-fix in the `ml_model$update()` method
- The default scoring method for `cv` now only switches to log-score+brier score
  when the response is a factor. Custom model-scoring function (cv argument
  modelscore) automatically gets 'weights' appended to the formal-arguments.

# targeted 0.4

- `alean`: Assumption Lean inference for generalized linear model parameters
- `ate` now supports general family argument
- `cate` now supports parallelization via the future or parallel package
- `ml_model` refactored. `ML` new wrapper for various machine learning models.
- `cv` parallelization (future or parallel package)
- `riskreg_cens` cumulative risk, restricted mean survival predictions (censored
  unbiased regression estimates)

# targeted 0.3

- Conditional average treatment estimator `cate`, `crr`
- Generic prediction model class `ml_model`
- design
- SuperLearner wrapper `SL`
- Average Treatment among responders `RATE`

# targeted 0.2.0

- Weighted Naive Bayes classifer with `NB`
- Pooled adjacent violator algorithm `pava`
- ODE solver `ode_solve`
- Calibration  `calibration`
- Cross-validation `cv`
- `ace` method updated and renamed to `ate`

# targeted 0.1.1

- Maintenance release.

# targeted 0.1

- Initialization of the new package `targeted` with implementation
  of augmented inverse probability weighting methods for estimation
  with missing data and causal inference (`aipw`, `ace`), and
  double robust methods for risk regression with binary exposure
  variables (`riskreg`).
