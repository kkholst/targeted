# Changelog

All notable changes to this project will be documented in this file.

## [0.6.0](https://github.com/kkholst/targeted/compare/v0.5..v0.6.0) - 2025-03-28

### Bug Fixes

- *(cate)* Now sets correct mc.cores argument in mclapply ([#24](https://github.com/NN-AI-Analytics/trialsim//issues/24)) - ([260ded8](https://github.com/kkholst/targeted/commit/260ded89cecb19f9e99f21f3124dc38e7be629aa))
- Scoring method only switches to log-score+brier score when the response is a factor. The model-scoring function (cv argument modelscore) automatically gets 'weights' appended to the formal-arguments. - ([f3acb5d](https://github.com/kkholst/targeted/commit/f3acb5dd8a3ede8f825486508b9330dddba61805))

### Developer

- *(RATE)* Fix linter and re-use roxygen documentation ([#45](https://github.com/NN-AI-Analytics/trialsim//issues/45)) - ([a31a37c](https://github.com/kkholst/targeted/commit/a31a37c30f8925e28bd3b5e776552da2dc596b9c))
- *(cate)* Add future deprecating warning for replaced argument types ([#26](https://github.com/NN-AI-Analytics/trialsim//issues/26)) - ([d2cb980](https://github.com/kkholst/targeted/commit/d2cb9806858b6d93696999cc994b52309036bdac))
- *(lintr)* Return_linter excludes files in inst/ + fix all remaining lint issues in R/ ([#44](https://github.com/NN-AI-Analytics/trialsim//issues/44)) - ([0f87889](https://github.com/kkholst/targeted/commit/0f878897ed9c0a33af1d678420808b58e6f7cf18))
- *(lintr)* Require explicit return expressions inside function ([#43](https://github.com/NN-AI-Analytics/trialsim//issues/43)) - ([31b19fc](https://github.com/kkholst/targeted/commit/31b19fc9fdb6be9885f726e9143fa03591234660))
- *(ml_model)* Adding intercept argument and refactoring design - ([fe5754c](https://github.com/kkholst/targeted/commit/fe5754cc77d18ccccc74d3a64f0986dad7d0d9fd))
- *(ml_model)* Minor documentation improvements + formatting ([#37](https://github.com/NN-AI-Analytics/trialsim//issues/37)) - ([4b4bee0](https://github.com/kkholst/targeted/commit/4b4bee09a1d8c780ca5a4a2ded4db7f142f4bf0c))
- Fixing github workflows + adding workflow to lint R package ([#33](https://github.com/NN-AI-Analytics/trialsim//issues/33)) - ([bcd50bd](https://github.com/kkholst/targeted/commit/bcd50bdbbc3b2ffb047efc4de77bc0a9dfcd2cd4))
- Adding custom  function to inform users about deprecated function arguments ([#32](https://github.com/NN-AI-Analytics/trialsim//issues/32)) - ([d0865a2](https://github.com/kkholst/targeted/commit/d0865a266c5e03659784cc7b5392b2b5dbccff09))
- Removing kalman components from src/target ([#31](https://github.com/NN-AI-Analytics/trialsim//issues/31)) - ([5251b00](https://github.com/kkholst/targeted/commit/5251b000154b5366288b1d82dce60662cb289927))
- Fix install rules in Makefile + fix pdf documentation of RATE.surv ([#27](https://github.com/NN-AI-Analytics/trialsim//issues/27)) - ([765ec8d](https://github.com/kkholst/targeted/commit/765ec8dc527e16df9fe702f3171cb0ac9572cbfb))
- Populate Makefile + minor repo cleaning ([#23](https://github.com/NN-AI-Analytics/trialsim//issues/23)) - ([fa39827](https://github.com/kkholst/targeted/commit/fa398273e10671c7c281479dafaf4b396e68d88c))
- Replace all ##' with #' ([#21](https://github.com/NN-AI-Analytics/trialsim//issues/21)) - ([d2f3c02](https://github.com/kkholst/targeted/commit/d2f3c024399e031debd6627c72f0609379de17b7))
- Use `rcmdcheck::rcmdcheck` in `r_check` make target for modern output - ([a02c5ca](https://github.com/kkholst/targeted/commit/a02c5ca40266387ec8f3e7931e2c0fd991401691))
- Fix failing `target_check.yaml` workflow - ([0a4adec](https://github.com/kkholst/targeted/commit/0a4adec8dc729b975d81e84175cb97a5619fbcd1))
- Removing `devtools::load_all` from test rule and use only `tinytest::test_package` ([#9](https://github.com/NN-AI-Analytics/trialsim//issues/9)) - ([56137f8](https://github.com/kkholst/targeted/commit/56137f8c7600fc5ff436a23773758616e399a458))
- Switch from `testthat` to `tinytest` for unit testing of R package ([#6](https://github.com/NN-AI-Analytics/trialsim//issues/6)) - ([be86072](https://github.com/kkholst/targeted/commit/be860727c8e53d5603fb5d7dced7b81b1af44bad))
- Adding `.lintr` config for R code linter - ([7fe7b56](https://github.com/kkholst/targeted/commit/7fe7b566eb77b80aed38e256b0d1f917a834020b))

### Features

- *(ml_model)* Allow argument updates for estimate and predict methods  ([#42](https://github.com/NN-AI-Analytics/trialsim//issues/42)) - ([a2405fd](https://github.com/kkholst/targeted/commit/a2405fdc95076b0c1e1fae819d032996a194d470))
- Adding `add_dots` utility function ([#2](https://github.com/NN-AI-Analytics/trialsim//issues/2)) - ([bb21da4](https://github.com/kkholst/targeted/commit/bb21da4c66fb3c8e9a3ce58677110b36976fb328))

### Cate

- Stratify argument and clean-up - ([a983c99](https://github.com/kkholst/targeted/commit/a983c996e2620139080dfda0932ce356318acc1c))
- Support for parallel computations - ([1a03dd3](https://github.com/kkholst/targeted/commit/1a03dd37fb8d2bbf6ed94a4ad0a270500205dd4a))

### Ml_model

- Clone (deep) now copies the environment of the estimation function (fitfun) - ([98b9b08](https://github.com/kkholst/targeted/commit/98b9b088a7dd35ca70c3ec61a2571e2704465183))


# 0.6
- Development version
- repeated cross-fitting in `cate` function via the new 'rep' argument
- First argument to `ml_model` can be a character defining the response-variable (optional)
- `predictor` wrapper, and `predictor_sl`, `predictor_glm`, ...

# 0.5
- `cate` now also returns the expected potential outcomes and influence functions
- Bug-fix in the `ml_model$update()` method
- The default scoring method for `cv` now only switches to log-score+brier score
  when the response is a factor. Custom model-scoring function (cv argument
  modelscore) automatically gets 'weights' appended to the formal-arguments.

# 0.4

- `alean`: Assumption Lean inference for generalized linear model parameters
- `ate` now supports general family argument
- `cate` now supports parallelization via the future or parallel package
- `ml_model` refactored. `ML` new wrapper for various machine learning models.
- `cv` parallelization (future or parallel package)
- `riskreg_cens` cumulative risk, restricted mean survival predictions (censored
  unbiased regression estimates)

# 0.3

- Conditional average treatment estimator `cate`, `crr`
- Generic prediction model class `ml_model`
- design
- SuperLearner wrapper `SL`
- Average Treatment among responders `RATE`

# 0.2.0

- Weighted Naive Bayes classifer with `NB`
- Pooled adjacent violator algorithm `pava`
- ODE solver `ode_solve`
- Calibration  `calibration`
- Cross-validation `cv`
- `ace` method updated and renamed to `ate`

# 0.1.1

- Maintenance release.

# 0.1

- Initialization of the new package `targeted` with implementation
  of augmented inverse probability weighting methods for estimation
  with missing data and causal inference (`aipw`, `ace`), and
  double robust methods for risk regression with binary exposure
  variables (`riskreg`).
