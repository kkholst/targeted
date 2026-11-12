# average treatment effect estimation with Missing Outcome Imputation

Estimates the average treatment effect in settings where the outcome may
be missing. The treatment effect implied by a parametric imputation
model is targeted directly through an efficient one-step estimator
(Nordland et al., 2026).

## Usage

``` r
moi(
  response.model,
  treatment.model,
  imputation.model,
  imputation.subset = NULL,
  data,
  type = c("direct", "indirect"),
  id = NULL,
  imputation.augmentation.model = NULL,
  missing.model = NULL,
  nfolds = 1,
  silent = FALSE,
  stratify = FALSE,
  mc.cores = NULL,
  second.order = TRUE,
  return.all = FALSE
)
```

## Arguments

- response.model:

  formula or learner object (formula =\> learner_glm). For the direct
  estimator, the outcome model for the imputed outcome
  \\\tilde{Y}(\theta)\\

- treatment.model:

  formula specifying the binary treatment. Only an intercept is allowed
  on the right-hand side, e.g. `a ~ 1`

- imputation.model:

  formula or learner_glm object (formula =\> learner_glm). Model
  \\U(X,A,Z;\theta)\\ used to impute the missing outcomes

- imputation.subset:

  character string with an R expression evaluating to a logical vector
  indicating the rows of `data` used to fit the imputation model, e.g.
  `"!is.na(y)"`. The expression is evaluated in the context of `data`.
  When `NULL` (default), all rows are used

- data:

  data.frame (`data.table` and `tbl_df` objects are coerced to
  `data.frame`)

- type:

  character specifying the estimator. `"direct"` (default) gives the
  direct one-step imputation estimator, which uses no missingness model.
  `"indirect"` gives the sub-target estimator, which requires
  `missing.model`

- id:

  (integer or character) optional subject id vector of length
  `nrow(data)`.

- imputation.augmentation.model:

  formula or learner object (formula =\> learner_glm); default `NULL`.
  Working model for the conditional imputation mean \\E\[U(X, A, Z;
  \theta) \mid W, A\]\\. When supplied, an augmentation term is added to
  the imputation sub-target for improved efficiency. Only used when
  `type = "indirect"`

- missing.model:

  formula or learner object (formula =\> learner_glm(family =
  binomial)); default `NULL`. Model for the probability of the outcome
  being missing, \\P(\Delta = 0 \| A = a)\\. Required when
  `type = "indirect"` and ignored when `type = "direct"`

- nfolds:

  number of folds (positive integer), or a pre-specified list of fold
  indices where each element is an integer vector of observation indices
  forming a partition of `1:nrow(data)`.

- silent:

  suppress all messages and progressbars

- stratify:

  if TRUE the response.model, missing.model and auxiliary.model (if
  provided) will be stratified by treatment

- mc.cores:

  (optional) number of cores. parallel::mcmapply used instead of future

- second.order:

  add seconder order term to IF to handle misspecification of outcome
  models

## Value

moi.targeted object

## Details

The `moi` function implements an estimator for the average treatment
effect where missing outcomes are imputed using a parametric (glm)
model.

The function estimate the target parameter

\$\$E\[\tilde{Y}\| A = 1\] - E\[\tilde{Y}\| A = 0\],\$\$

where

\$\$E\[\tilde{Y}\| A = a\] = E\[\Delta Y \| A=a\] + P(\Delta=0 \| A=a)
E\[U(X, A, Z; \theta) \| A=a, \Delta=0\],\$\$

and \\\Delta\\ denotes the non-missing indicator, and \\U\\ denotes the
imputation model possibly depending on baseline covariates \\X\\, the
treatment \\A\\, and a post randomization variable \\Z\\.

Inference is based on the estimated influence function of the associated
(covariate adjusted) one-step estimator.

Two estimators are available, selected by the `type` argument:

- **Direct one-step imputation estimator** (`type = "direct"`, the
  default): targets \\E\[\tilde{Y}(\theta) \| A = a\]\\ directly via a
  (covariate augmented) one-step estimator applied to the imputed
  outcome \\\tilde{Y} = \Delta Y + (1-\Delta) U(X,A,Z;\theta)\\, see
  supplementary material C of Nordland et al. (2026).

- **Sub-target (indirect) estimator** (`type = "indirect"`): estimates
  the three sub-target parameters \\E\[\Delta Y \| A=a\]\\, \\P(\Delta=0
  \| A=a)\\ and \\E\[U(X,A,Z;\theta) \| A=a, \Delta=0\]\\ separately and
  combines them. A missingness model is required for (augmented)
  estimation of \\P(\Delta=0 \| A=a)\\ and for the optional imputation
  model augmentation, see supplementary material B of Nordland et al.
  (2026).

## References

Nordland, A., Holst, K. K., Redek, D., Pipper, C. B. & Iversen, A. T.
(2026) One-step Outcome Imputation: An Alternative to Multiple
Imputation. arXiv: https://arxiv.org/abs/2606.07174.

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
## direct one-step imputation estimator
moi(response.model = y ~ a + w,
    treatment.model = a ~ 1,
    imputation.model = y ~ a + w,
    imputation.subset = "!is.na(y)",
    data = d)
#> Error in ic_theta[, 1]: incorrect number of dimensions

## sub-target estimator
moi(response.model = y ~ a + w,
    treatment.model = a ~ 1,
    imputation.model = y ~ a + w,
    imputation.subset = "!is.na(y)",
    missing.model = ~ a + w,
    data = d,
    type = "indirect")
#>                       Estimate Std.Err   2.5% 97.5%    P-value
#> E[ỹ(1)]                 1.9258 0.06130 1.8056 2.046 1.355e-216
#> E[ỹ(0)]                 1.0066 0.06168 0.8857 1.127  7.021e-60
#> [E[ỹ(1)]] - [E[ỹ(0)]]   0.9191 0.07466 0.7728 1.065  7.950e-35
```
