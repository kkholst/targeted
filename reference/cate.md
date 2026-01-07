# Conditional Average Treatment Effect estimation

Conditional Average Treatment Effect estimation with cross-fitting.

## Usage

``` r
cate(
  response.model,
  propensity.model,
  cate.model = ~1,
  calibration.model = NULL,
  data,
  contrast,
  nfolds = 1,
  rep = 1,
  silent = FALSE,
  stratify = FALSE,
  mc.cores = NULL,
  rep.type = c("nuisance", "average"),
  var.type = "IC",
  second.order = TRUE,
  response_model = deprecated,
  cate_model = deprecated,
  propensity_model = deprecated,
  treatment = deprecated,
  ...
)
```

## Arguments

- response.model:

  formula or learner object (formula =\> learner_glm)

- propensity.model:

  formula or learner object (formula =\> learner_glm)

- cate.model:

  formula specifying regression design for conditional average treatment
  effects

- calibration.model:

  linear calibration model. Specify covariates in addition to predicted
  potential outcomes to include in the calibration.

- data:

  data.frame

- contrast:

  treatment contrast (default 1 vs 0)

- nfolds:

  number of folds

- rep:

  number of replications of cross-fitting procedure

- silent:

  suppress all messages and progressbars

- stratify:

  if TRUE the response.model will be stratified by treatment

- mc.cores:

  (optional) number of cores. parallel::mcmapply used instead of future

- rep.type:

  repeated cross-fitting applied by averaging nuisance models
  (`rep.type="nuisance"`) or by average estimates from each replication
  (`rep.type="average"`).

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

  Deprecated. Use propensity.model instead.

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

## References

Mark J. van der Laan (2006) Statistical Inference for Variable
Importance, The International Journal of Biostatistics.

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
     propensity.model=a~w1+w2,
     data=d)
#>             Estimate Std.Err   2.5%  97.5%    P-value
#> E[y(1)]       1.8508 0.04186 1.7687 1.9328  0.000e+00
#> E[y(0)]       0.8279 0.02054 0.7876 0.8681  0.000e+00
#> ───────────                                          
#> (Intercept)   1.0229 0.04768 0.9295 1.1164 4.287e-102
## CATE
cate(cate.model=~1+w2,
     response.model=y~a*(w1+w2),
     propensity.model=a~w1+w2,
     data=d)
#>             Estimate Std.Err   2.5%  97.5%    P-value
#> E[y(1)]       1.8508 0.04186 1.7687 1.9328  0.000e+00
#> E[y(0)]       0.8279 0.02054 0.7876 0.8681  0.000e+00
#> ───────────                                          
#> (Intercept)   0.9999 0.04654 0.9087 1.0911 2.146e-102
#> w2            0.9879 0.04304 0.9035 1.0723 1.451e-116

if (FALSE)  ## superlearner example
mod1 <- list(
   glm = learner_glm(y~w1+w2),
   gam = learner_gam(y~s(w1) + s(w2))
)
s1 <- learner_sl(mod1, nfolds=5)
#> Error: object 'mod1' not found
cate(cate.model=~1,
     response.model=s1,
     propensity.model=learner_glm(a~w1+w2, family=binomial),
     data=d,
     stratify=TRUE)
#> Error: object 's1' not found
 # \dontrun{}
```
