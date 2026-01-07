# Construct stratified learner

This function creates a stratified learner from an existing
[learner](learner.md) wrapper function such as
[learner_glm](learner_glm.md) or [learner_xgboost](learner_xgboost.md).
The stratification variable can be specified either using the `stratify`
argument (which can be given as a string "a" or a formula , for example
~ I(a==0)), or it can be defined as a special term directly in the
formula, y ~ ... + stratify(a). The formula will subsequently be passed
to the `learner_` wrapper without the stratify special term.

## Usage

``` r
learner_stratify(
  formula,
  learner,
  stratify = NULL,
  info = NULL,
  learner.args = list(),
  ...
)
```

## Arguments

- formula:

  formula specifying outcome and design matrix

- learner:

  (learner) [learner](learner.md) object

- stratify:

  (character,formula) variables to stratify by

- info:

  optional description of the model

- learner.args:

  (list) optional arguments to the learner constructor

- ...:

  additional arguments passed to the learner constructor

## Value

learner object

## Examples

``` r
simdata <- function(n=1000) {
  a <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(-1 + a + a * x))
  data.frame(y, a, x)
}
d <- simdata()

lr <- learner_stratify(
  y ~ x + stratify(a),
  learner_glm,
  family=binomial()
)
lr$estimate(d)
lr$predict(head(d))
#> [1] 0.27517660 0.30335786 0.03560212 0.27836499 0.15517654 0.28005110
```
