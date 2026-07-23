# Average Treatment Effect Estimation

## Introduction

Consider observed data (Y, A, W) where Y is the outcome, A \in \\0,1\\
is the binary treatment, and W is a vector of covariates. Under the
potential outcomes framework, we define the *average treatment effect*
(ATE) as

\tau = E\[Y(1)\] - E\[Y(0)\]

where Y(a) denotes the outcome that would have been observed under
treatment A = a. Identification of \tau from observed data requires
standard causal assumptions: consistency, positivity, and no unmeasured
confounding.

The `cate` function in the `targeted` package estimates the ATE using
the *augmented inverse probability weighted* (AIPW) estimator, also
known as the doubly-robust (DR) *one-step estimator*. For each treatment
level a, the efficient influence function for the marginal mean \psi_a =
E\[Y(a)\] is

\varphi_a(Y, A, W) = \frac{I(A=a)}{\pi_a(W)}\\Y - \mu_a(W)\\ +
\mu_a(W) - \psi_a

where \pi_a(W) = P(A = a \mid W) is the propensity score and \mu_a(W) =
E\[Y \mid A = a, W\] is the outcome regression. The resulting estimator
is *doubly robust*: it is consistent if either \pi_a or \mu_a is
correctly specified. When both models are correctly specified, the
estimator achieves the semiparametric efficiency bound.

## Continuous outcome

### Simulated data

We generate data from a model with a known treatment effect. The outcome
depends on confounders W_1 and W_2, with an interaction between
treatment and W_2:

Y = \cos(W_1) + W_2 A + 0.2 W_2^2 + A + \varepsilon, \quad \varepsilon
\sim N(0,1)

The true ATE is E\[W_2 + 1\] = 1 since W_2 \sim N(0,1).

``` r

sim_data <- function(n = 2000) {
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a <- rbinom(n, 1, plogis(-1 + w1))
  y <- cos(w1) + w2 * a + 0.2 * w2^2 + a + rnorm(n)
  data.frame(y = y, a = a, w1 = w1, w2 = w2)
}

set.seed(2025)
d <- sim_data(2000)
```

### Parametric estimation

To estimate the ATE, we call `cate` with `cate.model = ~1` (an
intercept-only model, which targets the marginal ATE rather than
conditional effects). We specify parametric models for both the outcome
regression and the propensity score:

``` r

est <- cate(
  response.model = y ~ a * (w1 + w2),
  treatment.model = a ~ w1 + w2,
  cate.model = ~1,
  data = d
)
est
```

                Estimate Std.Err   2.5% 97.5%    P-value
    E[y(1)]       1.8149 0.09502 1.6287 2.001  2.550e-81
    E[y(0)]       0.7889 0.03423 0.7219 0.856 1.609e-117
    ───────────
    (Intercept)   1.0260 0.10395 0.8222 1.230  5.612e-23

The output shows estimates of the potential outcome means E\[Y(1)\] and
E\[Y(0)\], followed by the ATE (labelled `(Intercept)`). With correctly
specified parametric models and no cross-fitting, the AIPW estimator is
\sqrt{n}-consistent and asymptotically normal.

By default, `cate` includes a second-order correction term
(`second.order = TRUE`) that improves robustness to misspecification of
the outcome model when the propensity model is a GLM.

### Cross-fitting

When flexible or data-adaptive models are used for the nuisance
parameters, the AIPW estimator can suffer from overfitting bias.
*Cross-fitting* (sample splitting) resolves this: the data are
partitioned into K folds, nuisance models are fitted on K-1 folds and
predictions are made on the held-out fold. This relaxes the Donsker
conditions that would otherwise be required for \sqrt{n}-consistency.

With parametric models, cross-fitting is not strictly necessary but
introduces no harm:

``` r

est_cf <- cate(
  response.model = y ~ a * (w1 + w2),
  treatment.model = a ~ w1 + w2,
  cate.model = ~1,
  data = d,
  nfolds = 5
)
est_cf
```

                Estimate Std.Err   2.5%  97.5%    P-value
    E[y(1)]        1.827 0.08855 1.6538 2.0009  1.272e-94
    E[y(0)]        0.788 0.03445 0.7205 0.8555 7.843e-116
    ───────────
    (Intercept)    1.039 0.09805 0.8472 1.2315  2.969e-26

### Flexible nuisance models

One of the main strengths of the AIPW framework is that nuisance models
can be replaced with flexible or machine learning estimators without
sacrificing valid inference for the target parameter. The `learner`
class in `targeted` provides a unified interface for specifying these
models.

Here we use a generalized additive model (GAM) for the outcome
regression, stratified by treatment arm, combined with a logistic GLM
for the propensity score. Stratification (`stratify = TRUE`) fits
separate outcome models for treated and untreated units:

``` r

est_gam <- cate(
  response.model = learner_gam(y ~ s(w1) + s(w2)),
  treatment.model = learner_glm(a ~ w1 + w2, family = binomial),
  cate.model = ~1,
  data = d,
  nfolds = 5,
  stratify = TRUE
)
est_gam
```

                Estimate Std.Err   2.5%  97.5%    P-value
    E[y(1)]       1.8624 0.06805 1.7291 1.9958 6.554e-165
    E[y(0)]       0.7938 0.03067 0.7337 0.8539 1.029e-147
    ───────────
    (Intercept)   1.0687 0.07218 0.9272 1.2101  1.353e-49

For even greater flexibility, an ensemble learner (superlearner) can
combine multiple candidate models. The superlearner selects the optimal
convex combination of learners via cross-validation:

``` r

outcome_model <- learner_sl(
  list(
    glm = learner_glm(y ~ w1 * w2),
    gam = learner_gam(y ~ s(w1) + s(w2))
  ),
  nfolds = 5
)

est_sl <- cate(
  response.model = outcome_model,
  treatment.model = learner_glm(a ~ w1 + w2, family = binomial),
  cate.model = ~1,
  data = d,
  nfolds = 5,
  stratify = TRUE
)
est_sl
```

                Estimate Std.Err   2.5%  97.5%    P-value
    E[y(1)]       1.8499 0.06843 1.7158 1.9840 6.054e-161
    E[y(0)]       0.7964 0.03076 0.7361 0.8567 8.970e-148
    ───────────
    (Intercept)   1.0535 0.07249 0.9114 1.1956  7.400e-48

## Binary outcome

The AIPW estimator applies equally to binary outcomes. Here the ATE is a
*risk difference*: the difference in marginal probabilities of the event
under treatment versus control.

### Simulated data

We generate binary outcomes from a logistic model with a true log-odds
ratio of 0.8:

``` r

sim_binary <- function(n = 2000) {
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a <- rbinom(n, 1, plogis(-0.5 + 0.5 * w1))
  p1 <- plogis(-1 + 0.5 * w1 - 0.3 * w2 + 0.8 * a)
  y <- rbinom(n, 1, p1)
  data.frame(y = y, a = a, w1 = w1, w2 = w2)
}

set.seed(2025)
db <- sim_binary(3000)
```

### Estimation

``` r

est_bin <- cate(
  response.model = learner_glm(y ~ a * (w1 + w2), family = binomial),
  treatment.model = learner_glm(a ~ w1 + w2, family = binomial),
  cate.model = ~1,
  data = db,
  nfolds = 5
)
est_bin
```

                Estimate Std.Err   2.5%  97.5%    P-value
    E[y(1)]       0.4498 0.01495 0.4205 0.4791 5.632e-199
    E[y(0)]       0.2935 0.01080 0.2723 0.3146 9.915e-163
    ───────────
    (Intercept)   0.1563 0.01814 0.1208 0.1919  6.921e-18

The output shows the estimated marginal event probabilities E\[Y(1)\]
and E\[Y(0)\] and their difference (the risk difference). To obtain an
odds ratio, we can transform the potential outcome estimates using
[`lava::estimate`](https://kkholst.github.io/lava/reference/estimate.default.html):

``` r

or <- with(lava::estimate(est_bin$estimate),
           lava::logit(`E[y(1)]`) - lava::logit(`E[y(0)]`))
merge(or, exp(or), labels = c("log(OR)", "OR"))
```

            Estimate Std.Err   2.5%  97.5%   P-value
    log(OR)   0.6771 0.07842 0.5234 0.8308 5.876e-18
    OR        1.9682 0.15435 1.6657 2.2708 3.040e-37

## Conditional average treatment effects

The examples above all use `cate.model = ~1`, targeting the marginal
ATE. The `cate` function also supports estimation of *conditional*
average treatment effects (CATE) by specifying a richer projection
model. For instance, `cate.model = ~1 + w2` projects the
individual-level treatment effect onto a linear model in W_2, allowing
the estimated effect to vary across levels of W_2. See
[`?cate`](../reference/cate.md) for details and examples.

## Session info

``` r

sessionInfo()
```

    R version 4.6.1 (2026-06-24)
    Platform: aarch64-apple-darwin25.5.0
    Running under: macOS Tahoe 26.5.2

    Matrix products: default
    BLAS:   /Users/klaus/.asdf/installs/r/4.6.1/lib/R/lib/libRblas.dylib
    LAPACK: /Users/klaus/.asdf/installs/r/4.6.1/lib/R/lib/libRlapack.dylib;  LAPACK version 3.12.1

    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

    time zone: Europe/Copenhagen
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base

    other attached packages:
    [1] future_1.70.0 targeted_0.8

    loaded via a namespace (and not attached):
     [1] nlme_3.1-169           progressr_1.0.0        mets_1.3.12
     [4] cli_3.6.6              knitr_1.51             rlang_1.3.0
     [7] xfun_0.60              otel_0.2.0             jsonlite_2.0.0
    [10] future.apply_1.20.2    listenv_1.0.0          lava_1.9.2
    [13] htmltools_0.5.9        rmarkdown_2.31         quadprog_1.5-8
    [16] grid_4.6.1             evaluate_1.0.5         fastmap_1.2.0
    [19] numDeriv_2016.8-1.1    mvtnorm_1.4-2          yaml_2.3.12
    [22] timereg_2.0.7          compiler_4.6.1         codetools_0.2-20
    [25] Rcpp_1.1.2             mgcv_1.9-4             lattice_0.22-9
    [28] digest_0.6.39          R6_2.6.1               parallelly_1.48.0
    [31] parallel_4.6.1         splines_4.6.1          Matrix_1.7-5
    [34] RcppArmadillo_15.4.0-1 tools_4.6.1            globals_0.19.1
    [37] survival_3.8-6        
