# Construct a learner

Constructs a [learner](learner.md) class object for random survival
forests

## Usage

``` r
learner_surv_rf(
  formula,
  info = "survival forest (ranger)",
  num.threads = 1L,
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

- num.threads:

  Number of threads. Use 0 for all available cores. Default is 2 if not
  set by options/environment variables (see below).

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Further arguments passed to or from other methods (currently ignored).

## Value

[learner](learner.md) object.

## Author

Klaus Kähler Holst

## Examples

``` r
data(sTRACE, package="mets")
mod <- learner_surv_rf(Surv(time, status>0) ~ sex + age)
mod$estimate(sTRACE)
mod$predict(head(sTRACE), times=5) # P(T>t|X)
#> [1] 0.1813138 0.7495813 0.4379002 0.8956209 0.7800441 0.5206638
```
