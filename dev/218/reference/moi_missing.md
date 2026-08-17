# Mean Imputation Among Missing Outcomes

Estimates the mean of a given parametric imputation model among
observations with a missing outcome and a given treatment. Specifically,
it provides estimates of \\E\[U(X,A,Z;\theta)\|A=a, \Delta=0\]\\, for an
imputation model \\U\\, where \\X\\ denotes baseline covariates, \\A\\
denotes the treatment, \\Z\\ denotes post randomization covariates, and
\\\Delta\\ denotes a non-missing indicator. Influence function based
standard errors are also provided.

## Usage

``` r
moi_missing(
  data,
  id,
  delta,
  treatment.model,
  imputation.model,
  imputation.subset = NULL,
  imputation.augmentation = FALSE,
  missing.model = NULL,
  imputation.augmentation.model = NULL,
  extended.output = FALSE
)
```

## Arguments

- id:

  A vector with subject IDs

- treatment.model:

  A `learner` object for the binary treatment, used to extract the
  treatment variable and its levels.

- imputation.model:

  A learner object of class 'learner_glm' used to fit the imputation
  model. The learner must specify the outcome variable and model
  formula. If the learner was constructed with user-supplied `weights`,
  those weights are multiplied by the `imputation.subset` indicator
  (excluded rows receive zero weight).

- missing.model:

  `learner` object specifying the model for the probability of the
  outcome being observed/non-missing

- imputation.augmentation.model:

  `learner` object specifying the model for the imputation augmentation

- extended.output:

  Logical. If `TRUE`, the returned list also includes the augmentation
  component `IC3` of the influence function (only when
  `imputation.augmentation = TRUE`) and the imputation-model influence
  function `IC_epsilon`. Default is `FALSE`.

## Value

A list with components:

- estimate:

  A
  [`lava::estimate`](https://kkholst.github.io/lava/reference/estimate.default.html)
  object with coefficients \\E\[U\|A=1,\Delta=0\]\\ and
  \\E\[U\|A=0,\Delta=0\]\\ and the associated influence functions.

- imputation.model:

  The fitted imputation model.

- imputation.subset:

  The `imputation.subset` expression.

- levels:

  Treatment levels (character).

- IC3:

  (only if `extended.output = TRUE` and
  `imputation.augmentation = TRUE`) Named list (one entry per treatment
  level) giving the per-level augmentation contribution to the influence
  function.

- IC_epsilon:

  (only if `extended.output = TRUE`) Influence function for the
  imputation-model parameters.
