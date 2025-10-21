# targeted 0.6

This release introduces a new `learner` class replacing the previous `ML`
constructor.

- constructors for commonly used regression and classification models are also
  implemented: `learner_glm`, `learner_gam`, `learner_grf`, `learner_hal`,
  `learner_glmnet_cv`, `learner_svm`, `learner_xgboost`, `learner_mars`,
  `learner_isoreg`, `learner_naivebayes`
- new ensemble models (super-learners) available with `superlearner` and
    `learner_sl`
- `learner_stratify`: implementation of learner that can stratifies base-learner
  on categorical predictor
- `learner_expand_grid`: utility function to construct learners

Improved implementation of `cate` with repeated cross-fitting via the new 'rep'
argument. Linear calibration via the `calibration.model` argument [doi:10.1093/biomet/asaf029](https://doi.org/10.1093/biomet/asaf029).

Implementation of estimators for joint modelling of time-to-event (CIF) and
clinical outcome truncated by competing risk
[(arXiv.2502.03942)](https://doi.org/10.48550/arXiv.2502.03942):
`estimate_truncatedscore`.

`test_sw` Constrained least squares via Dykstra's algorithm, and fast signed wald test evaluation.

## Features

- *(test_sw)* signed wald intersection test
- *(cate)* linear calibration
- *(superlearner)*: standard meta-learner based on `quadprog::solve.QP`
- *(cv)* cross-validation `cv` method for superlearner objects ([#64](https://github.com/kkholst/targeted/issues/64)) - ([1d58b26](https://github.com/kkholst/targeted/commit/1d58b26190b979176c25be09c31e6ca1a892073b))
- *(design)* Fixing how specials is handled and passed to learner functions  - ([ab46749](https://github.com/kkholst/targeted/commit/ab46749cc2af1b593982f226aedbeb43a920520b))
- *(design)* Adding `print.design` ([#94](https://github.com/kkholst/targeted/issues/94)) - ([20eb170](https://github.com/kkholst/targeted/commit/20eb1704f6238cfeeceec1bd295fe5172d590e03))
- *(learner)* `learner_stratify` implementation of learner that can stratifies base-learner on categorical predictor  ([d561ea1](https://github.com/kkholst/targeted/commit/d561ea1b70f77b7db1d9bfcca2937c0a210c1468))
- *(learner)* [**breaking**] changing `formula` public field to active binding  ([#98](https://github.com/kkholst/targeted/issues/98)) - ([1505453](https://github.com/kkholst/targeted/commit/1505453c700fe0db921955336382f0b3b76489e8))
- *(learner)* [**breaking**] removing `response.arg` and `x.arg` arguments from `learner$new()` ([#92](https://github.com/kkholst/targeted/issues/92)) - ([4043dd7](https://github.com/kkholst/targeted/commit/4043dd743e90d8b79dda32f098528ed35f041d3b))
- *(learner)* adding new `summary` method to provide more details than `print` method ([#87](https://github.com/kkholst/targeted/issues/87)) - ([d12a581](https://github.com/kkholst/targeted/commit/d12a581246844e6f8ffe99a4beadcf20b52187b4))
- *(learner)* changed behaviour of `learner$design` to return not only 'x' matrix but everything including 'specials' ([#76](https://github.com/kkholst/targeted/issues/76)) - ([ca74abb](https://github.com/kkholst/targeted/commit/ca74abb876828f465bbd4e6613eb40ba5d754fa8))
- *(superlearner)* new ensemble models (super-learners) ([#104](https://github.com/kkholst/targeted/issues/104))
- *(learner)* `learner_expand_grid` utility function to construct learners ([#96](https://github.com/kkholst/targeted/issues/96)) - ([3ae461a](https://github.com/kkholst/targeted/commit/3ae461a657f78f2e1c79697113c3a9a5e21f7aa5))
- Generalized Additive Models `learner_gam` ([#77](https://github.com/kkholst/targeted/issues/77)) - ([de2ec2b](https://github.com/kkholst/targeted/commit/de2ec2b82c64f320601516591bddbf223de91e9a))
- Highly Adaptive Lasso `learner_hal` ([#75](https://github.com/kkholst/targeted/issues/75)) - ([62c4941](https://github.com/kkholst/targeted/commit/62c4941e808571d6574a8726ff335fe61c518490))
- Elastic net `learner_glmnet_cv` ([#74](https://github.com/kkholst/targeted/issues/74)) - ([67ba241](https://github.com/kkholst/targeted/commit/67ba2417ecaffa2371f075fa7da8e1e6dc531385))
- Generalized Linear Models `learner_glm`
  ([#63](https://github.com/kkholst/targeted/issues/63)) -
  ([0d2663a](https://github.com/kkholst/targeted/commit/0d2663acfa24eca38a90d2eff2769f0d1fb802d0))
- Naive Bayes classifier `learner_naivebayes` ([#88](https://github.com/kkholst/targeted/issues/88)) - ([2cbe979](https://github.com/kkholst/targeted/commit/2cbe979533bbb4d0722b303726db9f455dab2151))
- Generalized Random Forest `learner_grf` ([#84](https://github.com/kkholst/targeted/issues/84)) - ([82f76c8](https://github.com/kkholst/targeted/commit/82f76c86f21cba08053e75163b9fc7cc4e8f6ec0))
- Support Vector Regression `learner_svm` ([#83](https://github.com/kkholst/targeted/issues/83)) - ([4b28b30](https://github.com/kkholst/targeted/commit/4b28b30acf0e17888f7eb17447c0651ed82106f3))
- Isotonic regression `learner_isoreg` ([#82](https://github.com/kkholst/targeted/issues/82)) - ([e409b58](https://github.com/kkholst/targeted/commit/e409b58bd529a401e661c41902c150648b8265fe))
- XGBoost `learner_xgboost` ([#80](https://github.com/kkholst/targeted/issues/80)) - ([72ee414](https://github.com/kkholst/targeted/commit/72ee4148102f545f7a90747139d4a56e7a057913))
- Multivariate Adaptive Regression Splines `learner_mars` ([#79](https://github.com/kkholst/targeted/issues/79)) - ([0019060](https://github.com/kkholst/targeted/commit/0019060500ea7e1267edc7aaf41b73979383b7a0))
- Super-Learner `learner_sl` ([#78](https://github.com/kkholst/targeted/issues/78)) - ([03a81d2](https://github.com/kkholst/targeted/commit/03a81d2976a22d37eb0f559915fa09e716c4ad81))
- Adding new `learner` R6 class to replace `ml_model` ([#68](https://github.com/kkholst/targeted/issues/68)) - ([86c44fd](https://github.com/kkholst/targeted/commit/86c44fd6134ca785f84ebe4df62f175668db12e5))
- Improved `riskreg_cens` estimator ([#62](https://github.com/kkholst/targeted/issues/62)) - ([7aef75f](https://github.com/kkholst/targeted/commit/7aef75ff94946466212de3c88effdf204305f3ec))
- truncatedscore default is now to estimate P(T>=t) instead of CIF ([#46](https://github.com/kkholst/targeted/issues/46))
([b315645](https://github.com/kkholst/targeted/commit/b315645c4f4f0b8f29cebb6d777cadfde0b15222))
- *(cv)* silent arg ([#34](https://github.com/kkholst/targeted/issues/34)) - ([bb3d782](https://github.com/kkholst/targeted/commit/bb3d782107025699c0a7f08101f07c9118cddd2a))
- Feature/truncatedscore Implementation of estimators for joint modelling of
  time-to-event (CIF) and clinical outcome truncated by competing risk. ([#13](https://github.com/kkholst/targeted/issues/13))


## Documentation

- *(learner)* Harmonize documentation of all learner constructors  ([#93](https://github.com/kkholst/targeted/issues/93)) - ([1d37075](https://github.com/kkholst/targeted/commit/1d370757b094c9f1931b55a2cbc9985986a517fd))
- *(learner)* New vignette on prediction model class  ([#4](https://github.com/kkholst/targeted/issues/4)) - ([08bedd4](https://github.com/kkholst/targeted/commit/08bedd4f99bbc6c92baf369d024743aaab80592e))
- *(RATE)* Fix linter and re-use roxygen documentation ([#45](https://github.com/kkholst/targeted/issues/45)) - ([a31a37c](https://github.com/kkholst/targeted/commit/a31a37c30f8925e28bd3b5e776552da2dc596b9c))

## Bug Fixes

- *(cate)* Now sets correct mc.cores argument in mclapply ([#24](https://github.com/kkholst/targeted/issues/24)) - ([260ded8](https://github.com/kkholst/targeted/commit/260ded89cecb19f9e99f21f3124dc38e7be629aa))

## Developer

- Adding `add_dots` utility function ([#2](https://github.com/kkholst/targeted/issues/2)) - ([bb21da4](https://github.com/kkholst/targeted/commit/bb21da4c66fb3c8e9a3ce58677110b36976fb328))
- Adding .cliff.toml ([#47](https://github.com/kkholst/targeted/issues/47)) - ([47d7038](https://github.com/kkholst/targeted/commit/47d703874ea25ee152b4eaf90871301ceb1d2c30))
- Github workflows ([#33](https://github.com/kkholst/targeted/issues/33)) - ([bcd50bd](https://github.com/kkholst/targeted/commit/bcd50bdbbc3b2ffb047efc4de77bc0a9dfcd2cd4))
- Adding custom  function to inform users about deprecated function arguments ([#32](https://github.com/kkholst/targeted/issues/32)) - ([d0865a2](https://github.com/kkholst/targeted/commit/d0865a266c5e03659784cc7b5392b2b5dbccff09))
- Makefile + repository cleaning ([#23](https://github.com/kkholst/targeted/issues/23)) - ([fa39827](https://github.com/kkholst/targeted/commit/fa398273e10671c7c281479dafaf4b396e68d88c))
- Switch from `testthat` to `tinytest` for unit testing of R package ([#6](https://github.com/kkholst/targeted/issues/6)) - ([be86072](https://github.com/kkholst/targeted/commit/be860727c8e53d5603fb5d7dced7b81b1af44bad))
- Adding `.lintr` config for R code linter - ([7fe7b56](https://github.com/kkholst/targeted/commit/7fe7b566eb77b80aed38e256b0d1f917a834020b))

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

# targeted 0.2

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
