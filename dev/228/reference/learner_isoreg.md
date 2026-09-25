# Construct a learner

Constructs a [learner](learner.md) class object for isotonic regression
with [isoregw](pava.md).

## Usage

``` r
learner_isoreg(formula, info = "targeted::isoregw", learner.args = NULL, ...)
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

  Additional arguments to [isoregw](pava.md).

## Value

[learner](learner.md) object.

## Examples

``` r
x <- runif(5e3, -5, 5)
pr <- lava::expit(-1 + x)
y <- rbinom(length(pr), 1, pr)
d <- data.frame(y, x)

lr <- learner_isoreg(y ~ x)
lr$estimate(d)
pr_iso <- lr$predict(d)

if (interactive()) {
  plot(pr ~ x, cex=0.3)
  lines(sort(x), pr_iso[order(x)], col="red", type="s")
}
```
