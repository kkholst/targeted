# Assumption Lean inference for generalized linear model parameters

Assumption lean inference via cross-fitting (Double ML). See
\<doi:10.1111/rssb.12504

## Usage

``` r
alean(
  response_model,
  exposure_model,
  data,
  link = "identity",
  g_model,
  nfolds = 1,
  silent = FALSE,
  mc.cores,
  ...
)
```

## Arguments

- response_model:

  formula or [learner](learner.md) object (formula =\> glm)

- exposure_model:

  model for the exposure

- data:

  data.frame

- link:

  Link function (g)

- g_model:

  Model for \\E\[g(Y\|A,W)\|W\]\\

- nfolds:

  Number of folds

- silent:

  supress all messages and progressbars

- mc.cores:

  mc.cores Optional number of cores. parallel::mcmapply used instead of
  future

- ...:

  additional arguments to future.apply::future_mapply

## Value

alean.targeted object

## Details

Let \\Y\\ be the response variable, \\A\\ the exposure and \\W\\
covariates. The target parameter is: \$\$\Psi(P) = \frac{E(Cov\[A,
g\\E(Y\|A,W)\\\mid W\])} {E\\Var(A\mid W)\\} \$\$

The `response_model` is the model for \\E(Y\|A,W)\\, and
`exposure_model` is the model for \\E(A\|W)\\. `link` specifies \\g\\.

## Author

Klaus Kähler Holst

## Examples

``` r

sim1 <- function(n, family=gaussian(), ...) {
   m <- lava::lvm() |>
     lava::distribution(~y, value=lava::binomial.lvm()) |>
     lava::regression('a', value=function(l) l) |>
     lava::regression('y', value=function(a,l) a + l)
     if (family$family=="binomial")
        lava::distribution(m, ~a) <- lava::binomial.lvm()
   lava::sim(m, n)
}

library(splines)
f <- binomial()
d <- sim1(1e4, family=f)
e <- alean(
 response_model=learner_glm(y ~ a + bs(l, df=3), family=binomial),
 exposure_model=learner_glm(a ~ bs(l, df=3), family=f),
 data=d,
 link = "logit", mc.cores=1, nfolds=1
)
e
#>   Estimate Std.Err 2.5% 97.5%   P-value
#> a    1.104 0.05301    1 1.208 2.354e-96

e <- alean(response_model=learner_glm(y ~ a + l, family=binomial),
           exposure_model=learner_glm(a ~ l),
           data=d,
           link = "logit", mc.cores=1, nfolds=1)
e
#>   Estimate Std.Err   2.5% 97.5%   P-value
#> a      1.1 0.05284 0.9969 1.204 2.488e-96
```
