# Construct a learner

Constructs [learner](learner.md) class object for fitting generalized
additive models with [mgcv::gam](https://rdrr.io/pkg/mgcv/man/gam.html).

## Usage

``` r
learner_gam(
  formula,
  info = "mgcv::gam",
  family = gaussian(),
  select = FALSE,
  gamma = 1,
  learner.args = NULL,
  ...
)
```

## Arguments

- formula:

  (formula) Formula specifying response and design matrix.

- info:

  (character) Optional information to describe the instantiated
  [learner](learner.md) object.

- family:

  This is a family object specifying the distribution and link to use in
  fitting etc (see [`glm`](https://rdrr.io/r/stats/glm.html) and
  [`family`](https://rdrr.io/r/stats/family.html)). See
  [`family.mgcv`](https://rdrr.io/pkg/mgcv/man/family.mgcv.html) for a
  full list of what is available, which goes well beyond exponential
  family. Note that `quasi` families actually result in the use of
  extended quasi-likelihood if `method` is set to a RE/ML method
  (McCullagh and Nelder, 1989, 9.6).

- select:

  If this is `TRUE` then `gam` can add an extra penalty to each term so
  that it can be penalized to zero. This means that the smoothing
  parameter estimation that is part of fitting can completely remove
  terms from the model. If the corresponding smoothing parameter is
  estimated as zero then the extra penalty has no effect. Use `gamma` to
  increase level of penalization.

- gamma:

  Increase this beyond 1 to produce smoother models. `gamma` multiplies
  the effective degrees of freedom in the GCV or UBRE/AIC. `n/gamma` can
  be viewed as an effective sample size in the GCV score, and this also
  enables it to be used with REML/ML. Ignored with P-RE/ML or the `efs`
  optimizer.

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to
  [mgcv::gam](https://rdrr.io/pkg/mgcv/man/gam.html).

## Value

[learner](learner.md) object.

## Examples

``` r
n <- 5e2
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
d0 <- data.frame(y, x1, x2)

lr <- learner_gam(y ~ s(x1) + x2)
lr$estimate(d0)
if (interactive()) {
  plot(lr$fit)
}
```
