# Construct a learner

Constructs a [learner](learner.md) class object for fitting multivariate
adaptive regression splines with
[earth::earth](https://rdrr.io/pkg/earth/man/earth.html).

## Usage

``` r
learner_mars(
  formula,
  info = "earth::earth",
  degree = 1,
  nprune = NULL,
  glm = NULL,
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

- degree:

  Maximum degree of interaction (Friedman's \\mi\\). Default is `1`,
  meaning build an additive model (i.e., no interaction terms).

- nprune:

  Maximum number of terms (including intercept) in the pruned model.
  Default is NULL, meaning all terms created by the forward pass (but
  typically not all terms will remain after pruning). Use this to
  enforce an upper bound on the model size (that is less than `nk`), or
  to reduce exhaustive search time with `pmethod="exhaustive"`.  
    
  **The following arguments are for cross validation.**

- glm:

  NULL (default) or a list of arguments to pass on to
  [`glm`](https://rdrr.io/r/stats/glm.html). See the documentation of
  [`glm`](https://rdrr.io/r/stats/glm.html) for a description of these
  arguments See “*Generalized linear models*” in the vignette.
  Example:  
  `earth(survived~., data=etitanic, degree=2, glm=list(family=binomial))`  
    
  **The following arguments are for the forward pass.**

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to
  [earth::earth](https://rdrr.io/pkg/earth/man/earth.html).

## Value

[learner](learner.md) object.

## Examples

``` r
# poisson regression
n <- 5e2
x <- rnorm(n)
w <- 50 + rexp(n, rate = 1 / 5)
y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
d0 <- data.frame(y, x, w)

lr <- learner_mars(y ~ x + offset(log(w)), degree = 2,
  glm = list(family = poisson())
)
lr$estimate(d0)
lr$predict(data.frame(x = 0, w = c(1, 2)))
#> [1]  7.321076 14.642153
```
