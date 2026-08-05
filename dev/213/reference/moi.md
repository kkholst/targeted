# Average Treatment Effect Estimation with Missing Outcome Imputation

Estimates the Average Treatment Effect (ATE) in settings where the
outcome may be missing (not observed for all individuals). The treatment
effect implied by a parametric imputation model is targeted directly
through an efficient one-step estimator constructed from its influence
function (Nordland et al., 2026).

## Usage

``` r
moi(
  data,
  response.model,
  treatment.model,
  missing.model,
  imputation.model,
  imputation.subset = NULL,
  imputation.augmentation = FALSE,
  imputation.augmentation.model = NULL,
  return.all = FALSE,
  nfolds = 1,
  silent = FALSE,
  stratify = FALSE,
  mc.cores = NULL,
  second.order = TRUE
)
```

## Arguments

- data:

  A `data.frame` containing all variables required by the models.
  `data.table` and `tbl_df` objects are automatically coerced to
  `data.frame`.

- response.model:

  A `formula` or `learner` object specifying the response/outcome and
  the associated baseline adjusted model. If a `formula` is provided, it
  is automatically wrapped in [`learner_glm`](learner_glm.md). Used to
  estimate \\E\[\Delta Y \| A = a\]\\.

- treatment.model:

  A base R `stats` formula specifying the binary treatment variable.
  Only an intercept is allowed on the right-hand side, e.g., `A ~ 1`.

- missing.model:

  A `formula` or `learner` object specifying the model for the
  probability of the outcome being observed/non-missing (i.e.,
  \\P(\Delta = 1 \| A = a)\\). If a `formula` is provided, it is wrapped
  in `learner_glm(..., family = binomial())`. Used to estimate
  \\P(\Delta = 0 \| A = a)\\.

- imputation.model:

  A `formula` or `learner_glm` object specifying the missing outcome
  imputation model. If a `formula` is provided, it is wrapped in
  [`learner_glm`](learner_glm.md). Used to estimate \\E\[U(X, A, Z;
  \theta) \| A = a, \Delta = 0\]\\.

- imputation.subset:

  Optional character string giving an R expression that evaluates to a
  logical vector indicating which rows of `data` to use when fitting the
  imputation model. The expression is parsed and evaluated in the
  context of `data`; for example, `imputation.subset = "!is.na(y)"`
  restricts the fit to the observed outcomes. If `NULL` (default), all
  rows are used.

- imputation.augmentation:

  Logical. If `TRUE`, an augmentation term is added to the imputation
  estimator for improved efficiency. Default is `FALSE`.

- imputation.augmentation.model:

  A `formula`, `learner`, or `NULL` specifying a working model for the
  conditional imputation mean \\E\[U(X, A, Z; \theta) \mid W, A\]\\,
  used to augment the imputation estimator. Only used when
  `imputation.augmentation = TRUE`; if `NULL`, the imputation model
  \\U\\ itself is used. Default is `NULL`.

- return.all:

  Logical. If `TRUE`, the returned object includes all intermediate
  estimates in addition to the final ATE estimate. Default is `FALSE`.

- nfolds:

  number of folds (positive integer), or a pre-specified list of fold
  indices where each element is an integer vector of observation indices
  forming a partition of `1:nrow(data)`.

- silent:

  suppress all messages and progressbars

- stratify:

  if TRUE the response.model will be stratified by treatment

- mc.cores:

  (optional) number of cores. parallel::mcmapply used instead of future

- second.order:

  add seconder order term to IF to handle misspecification of outcome
  models

## Value

An object of class `moi.targeted` (inheriting from `targeted`), a list
with components:

- call:

  The matched call.

- estimate:

  A
  [`lava::estimate`](https://kkholst.github.io/lava/reference/estimate.default.html)
  object containing the per-arm expected potential outcomes
  \\E\[\tilde{Y}\|A=a\]\\ and the ATE contrast \\E\[\tilde{Y}\|A=1\] -
  E\[\tilde{Y}\|A=0\]\\, with influence-function-based standard errors.
  Row labels follow the [`cate`](cate.md) convention: per-arm rows are
  labeled \\E\[\tilde{y}(1)\]\\ and \\E\[\tilde{y}(0)\]\\ (or
  `E[tildeY(1)]` / `E[tildeY(0)]` in non-UTF-8 locales), and the
  contrast row is labeled \\E\[\tilde{y}(1)\]-E\[\tilde{y}(0)\]\\.

- levels:

  Treatment levels (character).

- intermediate:

  (only if `return.all = TRUE`) Intermediate estimates: \\E\[\Delta
  Y\|A=a\]\\, \\P(\Delta=0\|A=a)\\, and \\E\[U\|A=a, \Delta=0\]\\.

Standard methods (`print`, `summary`, `coef`, `vcov`, `IC`) are
provided.

## Details

The `moi` function implements an estimator for the Average Treatment
Effect where missing outcomes are imputed using a parametric (glm)
model.

The function estimate the target parameter

\$\$E\[\tilde{Y}\| A = 1\] - E\[\tilde{Y}\| A = 0\],\$\$

where

\$\$E\[\tilde{Y}\| A = a\] = E\[\Delta Y \| A=a\] + P(\Delta=0 \| A=a)
\cdot E\[U(X, A, Z; \theta) \| A=a, \Delta=0\],\$\$

and \\\Delta\\ denotes the non-missing indicator, and \\U\\ denotes the
imputation model possibly depending on baseline covariates \\X\\, the
treatment \\A\\, and a post randomization variable \\Z\\.

Inference in based on the estimated influence functions (IFs) of the
associated (covariate adjusted) one-step estimators.

When `imputation.augmentation = TRUE`, an augmentation term built from
the efficient influence function is added, giving an efficient one-step
estimator of the treatment effect implied by the imputation model
(Nordland et al., 2026). The augmentation uses a working model for the
conditional imputation mean \\E\[U(X, A, Z; \theta) \mid W, A\]\\ (see
`imputation.augmentation.model`) together with the missingness model.

If no observations are missing in an arm \\a\\, the imputation
contribution for that arm vanishes (\\P(\Delta = 0 \| A = a) = 0\\) and
\\E\[\tilde{Y} \| A = a\] = E\[Y \| A = a\]\\. If no observations are
missing in any arm, `moi` reduces to a standard [`cate`](cate.md) call
with `cate.model = ~ 1`.

## References

Nordland, A., Holst, K. K., Redek, D., Pipper, C. B. & Iversen, A. T.
(2026) One-step Outcome Imputation: An Alternative to Multiple
Imputation. arXiv: https://arxiv.org/abs/2606.07174.

## See also

[`cate`](cate.md) for Conditional Average Treatment Effect estimation,
[`learner`](learner.md) for creating learner objects,
[`lava::estimate`](https://kkholst.github.io/lava/reference/estimate.default.html)
for combining and transforming estimators

## Author

Andreas Nordland

## Examples

``` r
sim_moi <- function(n = 1000, ...) {
  w <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  y <- 1 + a + w + rnorm(n)
  ## outcome observed (delta = 1) with probability depending on w
  delta <- rbinom(n, 1, lava::expit(1 + w))
  y[delta == 0] <- NA
  data.frame(y, a, w)
}

d <- sim_moi(1000)
## ATE with missing outcomes imputed by a working glm model
moi(data = d,
    response.model = y ~ a + w,
    treatment.model = a ~ 1,
    missing.model = ~ a + w,
    imputation.model = y ~ a + w,
    imputation.subset = "!is.na(y)")
#>                       Estimate Std.Err   2.5% 97.5%    P-value
#> E[ỹ(1)]                 2.0214 0.06499 1.8941 2.149 2.236e-212
#> E[ỹ(0)]                 0.8962 0.06072 0.7771 1.015  2.705e-49
#> [E[ỹ(1)]] - [E[ỹ(0)]]   1.1253 0.07586 0.9766 1.274  8.823e-50
```
