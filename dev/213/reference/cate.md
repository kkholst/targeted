# Conditional Average Treatment Effect estimation

Conditional Average Treatment Effect estimation with cross-fitting.

## Usage

``` r
cate(
  response.model,
  treatment.model,
  cate.model = ~1,
  calibration.model = NULL,
  missing.model = NULL,
  data,
  contrast,
  nfolds = 1,
  rep = 1,
  id = NULL,
  silent = FALSE,
  stratify = FALSE,
  mc.cores = NULL,
  var.type = "IC",
  second.order = TRUE,
  response_model = deprecated,
  cate_model = deprecated,
  propensity_model = deprecated,
  propensity.model = deprecated,
  treatment = deprecated,
  ...
)
```

## Arguments

- response.model:

  formula or learner object (formula =\> learner_glm)

- treatment.model:

  formula or learner object (formula =\> learner_glm)

- cate.model:

  formula specifying regression design for conditional average treatment
  effects

- calibration.model:

  linear calibration model. Specify covariates in addition to predicted
  potential outcomes to include in the calibration.

- missing.model:

  formula or learner object; default `NULL`. Model for the missingness
  mechanism \\P(R=1 \mid X, A)\\. Required when the outcome in
  `response.model` contains NAs. If the formula LHS is omitted, the
  observation indicator is used automatically. When `stratify = TRUE`
  the missing model is fit separately per treatment arm. When supplied,
  the AIPW score is inverse-probability-of-observation weighted and (if
  `second.order = TRUE`) an additional second-order term is added to the
  influence function.

- data:

  data.frame

- contrast:

  treatment contrast (default 1 vs 0)

- nfolds:

  number of folds (positive integer), or a pre-specified list of fold
  indices where each element is an integer vector of observation indices
  forming a partition of `1:nrow(data)`.

- rep:

  number of replications of cross-fitting procedure by averaging
  estimates and influence functions from each replication

- id:

  (integer or character) optional subject id vector of length
  `nrow(data)`. The `id` can also be specified as part of the
  `cate.model` argument with a formula syntax: `~ 1 + cluster(id)`.

- silent:

  suppress all messages and progressbars

- stratify:

  if TRUE the response.model will be stratified by treatment

- mc.cores:

  (optional) number of cores. parallel::mcmapply used instead of future

- var.type:

  when equal to "IC" the asymptotic variance is derived from the
  influence function. Otherwise, based on expressions in Bannick et
  al. (2025) valid under different covariate-adaptive randomization
  schemes (only available for ATE and when `calibration.model` is also
  specified)

- second.order:

  add seconder order term to IF to handle misspecification of outcome
  models

- response_model:

  Deprecated. Use response.model instead.

- cate_model:

  Deprecated. Use cate.model instead.

- propensity_model:

  Deprecated. Use treatment.model instead.

- propensity.model:

  Deprecated. Use treatment.model instead.

- treatment:

  Deprecated. Use cate.model instead.

- ...:

  additional arguments to future.apply::future_mapply

## Value

cate.targeted object

## Details

We have observed data \\(Y,A,W)\\ where \\Y\\ is the response variable,
\\A\\ the binary treatment, and \\W\\ covariates. We further let \\V\\
be a subset of the covariates. Define the conditional potential mean
outcome \$\$\psi\_{a}(P)(V) = E\_{P}\[E\_{P}(Y\mid A=a, W)\|V\]\$\$ and
let \\m(V; \beta)\\ denote a parametric working model, then the target
parameter is the mean-squared error \$\$\beta(P) =
\operatorname{argmin}\_{\beta}
E\_{P}\[\\\Psi\_{1}(P)(V)-\Psi\_{0}(P)(V)\\ - m(V; \beta)\]^{2}\$\$

Missing data is handled under a Missing At Random assumption (MAR). Let
\\R\\ denote the indicator for data not being missing, \\R\perp
Y\|W,A\\. The nuisance models are \\Q(w,a) = E(Y\|W=w, A=a)\\, \\g_a(w)
= P(A=a\|W=w)\\, and \\\rho(w, a) = P(R=1\|W=w, A=a)\\. For the expected
potential outcome \\E\[Y(a)\]\\, the AIPW estimator then takes the form
\$\$\frac{1}{n}\sum\_{i=1}^n R_i I(A_i=a) / \\g_a(W_i) \rho(W_i, a)\\
(Y_i - Q(W_i,a)) + Q(W_i, a)\$\$.

## References

Mark J. van der Laan (2006) Statistical Inference for Variable
Importance, The International Journal of Biostatistics.

Bannick, Shao & Liu et al. (2025) A General Form of Covariate Adjustment
in Clinical Trials under Covariate-Adaptive Randomization, Biometrika.

## Author

Klaus Kähler Holst, Andreas Nordland

## Examples

``` r
sim1 <- function(n=1000, ...) {
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a <- rbinom(n, 1, plogis(-1 + w1))
  y <- cos(w1) + w2*a + 0.2*w2^2 + a + rnorm(n)
  data.frame(y, a, w1, w2)
}

d <- sim1(5000)
## ATE
cate(cate.model=~1,
     response.model=y~a*(w1+w2),
     treatment.model=a~w1+w2,
     data=d)
#>             Estimate Std.Err   2.5%  97.5%    P-value
#> E[y(1)]       1.8047 0.04831 1.7100 1.8994 2.099e-305
#> E[y(0)]       0.8308 0.01984 0.7919 0.8697  0.000e+00
#> ───────────                                          
#> (Intercept)   0.9740 0.05337 0.8694 1.0786  2.054e-74
## CATE
cate(cate.model=~1+w2,
     response.model=y~a*(w1+w2),
     treatment.model=a~w1+w2,
     data=d)
#>             Estimate Std.Err   2.5%  97.5%    P-value
#> E[y(1)]       1.8047 0.04831 1.7100 1.8994 2.099e-305
#> E[y(0)]       0.8308 0.01984 0.7919 0.8697  0.000e+00
#> ───────────                                          
#> (Intercept)   0.9502 0.05280 0.8467 1.0536  2.093e-72
#> w2            1.0377 0.04756 0.9445 1.1309 1.586e-105

if (FALSE)  ## superlearner example
mod1 <- list(
   glm = learner_glm(y~w1+w2),
   gam = learner_gam(y~s(w1) + s(w2))
)
s1 <- learner_sl(mod1, nfolds=5)
#> Error: object 'mod1' not found
cate(cate.model=~1,
     response.model=s1,
     treatment.model=learner_glm(a~w1+w2, family=binomial),
     data=d,
     stratify=TRUE)
#> Error: object 's1' not found
 # \dontrun{}

## Missing data
sim_missing_cate <- function(n = 5000, seed = 1) {
  set.seed(seed)
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a  <- rbinom(n, 1, 0.5) # randomized trial
  y_full <- 1 + a + w1 + 0.5 * w2 + rnorm(n)
  pR <- plogis(0.5 - 1 * w2 * a + 0.5 * a)
  R  <- rbinom(n, 1, pR)
  y  <- ifelse(R == 1, y_full, NA_real_)
  data.frame(y0 = y_full, y = y, a = a, w1 = w1, w2 = w2)
}
d <- sim_missing_cate()

# ignoring missing data (complete-case analysis)
cate(cate.model = ~1,
     response.model = y ~ a * w2, # wrong outcome model
     treatment.model = a ~ 1,
     data = na.omit(d), nfolds = 1L)
#>             Estimate Std.Err   2.5%  97.5%    P-value
#> E[y(1)]       1.9271 0.03483 1.8588 1.9953  0.000e+00
#> E[y(0)]       0.9076 0.03607 0.8370 0.9783 9.391e-140
#> ───────────                                          
#> (Intercept)   1.0194 0.04879 0.9238 1.1150  6.127e-97
# MAR analysis
fit <- cate(cate.model = ~1,
            response.model = y ~ a * w2,
            treatment.model = a ~ 1,
            missing.model  = ~ a * (w1 + w2),
            data = d, nfolds = 1L)
fit
#>             Estimate Std.Err   2.5% 97.5%    P-value
#> E[y(1)]       1.9807 0.03372 1.9146 2.047  0.000e+00
#> E[y(0)]       0.9725 0.03237 0.9091 1.036 2.485e-198
#> ───────────                                         
#> (Intercept)   1.0082 0.04573 0.9186 1.098 1.033e-107
```
