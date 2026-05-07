# Construct a learner

Constructs a [learner](learner.md) class object for fitting generalized
linear models with [stats::glm](https://rdrr.io/r/stats/glm.html) and
[MASS::glm.nb](https://rdrr.io/pkg/MASS/man/glm.nb.html). Negative
binomial regression is supported with `family = "nb"` (or alternatively
`family = "negbin"`).

## Usage

``` r
learner_glm(
  formula,
  info = "glm",
  family = gaussian(),
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

  a description of the error distribution and link function to be used
  in the model. For `glm` this can be a character string naming a family
  function, a family function or the result of a call to a family
  function. For `glm.fit` only the third option is supported. (See
  [`family`](https://rdrr.io/r/stats/family.html) for details of family
  functions.)

- learner.args:

  (list) Additional arguments to [learner\$new()](learner.md).

- ...:

  Additional arguments to [stats::glm](https://rdrr.io/r/stats/glm.html)
  or [MASS::glm.nb](https://rdrr.io/pkg/MASS/man/glm.nb.html).

## Value

[learner](learner.md) object.

## Examples

``` r
n <- 5e2
x <- rnorm(n)
w <- 50 + rexp(n, rate = 1 / 5)
y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
d0 <- data.frame(y, x, w)

lr <- learner_glm(y ~ x) # linear Gaussian model
lr$estimate(d0)
coef(lr$fit)
#> (Intercept)           x 
#>    434.8400    152.0976 

# negative binomial regression model with offset (using MASS::glm.nb)
lr <- learner_glm(y ~ x + offset(log(w)), family = "nb")
lr$estimate(d0)
coef(lr$fit)
#> (Intercept)           x 
#>    2.005382    0.389838 
lr$predict(data.frame(x = 1, w = c(1, 5))) # response scale
#>        1        2 
#> 10.97061 54.85304 
lr$predict(data.frame(x = 1, w = c(1, 5)), type = "link") # link scale
#>        1        2 
#> 2.395220 4.004658 
```
