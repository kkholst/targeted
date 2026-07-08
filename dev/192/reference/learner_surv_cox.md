# Construct a learner

Constructs a [learner](learner.md) class object for fitting Cox
proportional hazards models.

## Usage

``` r
learner_surv_cox(formula, info = "mets::phreg", learner.args = NULL, ...)
```

## Arguments

- formula:

  (formula) Formula specifying response and design matrix.

- info:

  (character) Optional information to describe the instantiated
  [learner](learner.md) object.

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to lower level funtions

## Value

[learner](learner.md) object.

## Author

Klaus Kähler Holst

## Examples

``` r
data(sTRACE, package="mets")
mod <- learner_surv_cox(Surv(time, status>0) ~ sex + strata(age))
mod$estimate(sTRACE)
mod$predict(head(sTRACE), times=5) # P(T>t|X)
#> [1] 0.3678794 1.0000000 0.3678794 1.0000000 1.0000000 0.3678794
```
