# AIPW (doubly-robust) estimator for Average Treatment Effect

Augmented Inverse Probability Weighting estimator for the Average
(Causal) Treatment Effect. All nuisance models are here parametric
(glm). For a more general approach see the `cate` implementation. In
this implementation the standard errors are correct even when the
nuisance models are mis-specified (the influence curve is calculated
including the term coming from the parametric nuisance models). The
estimate is consistent if either the propensity model or the outcome
model / Q-model is correctly specified.

## Usage

``` r
ate(
  formula,
  data = parent.frame(),
  weights,
  offset,
  family = stats::gaussian(identity),
  nuisance = NULL,
  propensity = nuisance,
  all,
  labels = NULL,
  adjust.nuisance = TRUE,
  adjust.propensity = TRUE,
  ...
)
```

## Arguments

- formula:

  formula (see details below)

- data:

  data.frame

- weights:

  optional frequency weights

- offset:

  optional offset (character or vector). can also be specified in the
  formula.

- family:

  Exponential family argument for outcome model

- nuisance:

  outcome regression formula (Q-model)

- propensity:

  propensity model formula

- all:

  when TRUE all standard errors are calculated (default TRUE when
  exposure only has two levels)

- labels:

  optional treatment labels

- adjust.nuisance:

  adjust for uncertainty due to estimation of outcome regression model
  parameters

- adjust.propensity:

  adjust for uncertainty due to estimation of propensity regression
  model parameters

- ...:

  additional arguments to lower level functions

## Value

An object of class '`ate.targeted`' is returned. See
[`targeted-class`](targeted-class.md) for more details about this class
and its generic functions.

## Details

The formula may either be specified as: response ~ treatment \|
nuisance-formula \| propensity-formula

For example: `ate(y~a | x+z+a | x*z, data=...)`

Alternatively, as a list: `ate(list(y~a, ~x+z, ~x*z), data=...)`

Or using the nuisance (and propensity argument):
`ate(y~a, nuisance=~x+z, ...)`

## See also

cate

## Author

Klaus K. Holst

## Examples

``` r
m <- lava::lvm(y ~ a+x, a~x) |>
     lava::distribution(~y, value = lava::binomial.lvm()) |>
     lava::ordinal(K=4, ~a) |>
     transform(~a, value = factor)
d <- lava::sim(m, 1e3, seed=1)
# (a <- ate(y~a|a*x|x, data=d))
(a <- ate(y~a, nuisance=~a*x, propensity=~x, data = d))
#>     Estimate Std.Err   2.5%  97.5%    P-value
#> a=0   0.2302 0.05278 0.1268 0.3337  1.288e-05
#> a=1   0.3148 0.05466 0.2077 0.4219  8.413e-09
#> a=2   0.4961 0.04732 0.4034 0.5889  1.019e-25
#> a=3   0.6348 0.02492 0.5860 0.6836 3.667e-143

# Comparison with randomized experiment
m0 <- lava::cancel(m, a~x)
lm(y~a-1, lava::sim(m0,2e4))
#> 
#> Call:
#> lm(formula = y ~ a - 1, data = lava::sim(m0, 20000))
#> 
#> Coefficients:
#>     a0      a1      a2      a3  
#> 0.2168  0.3467  0.4051  0.6188  
#> 

# Choosing a different contrast for the association measures
summary(a, contrast=c(2,4))
#> 
#> Augmented Inverse Probability Weighting estimator
#>   Response y (Outcome model: gaussian):
#>   y ~ a * x 
#>   Exposure a (Propensity model: logistic regression):
#>   a ~ x 
#> 
#>     Estimate Std.Err   2.5%  97.5%    P-value
#> a=0   0.2302 0.05278 0.1268 0.3337  1.288e-05
#> a=1   0.3148 0.05466 0.2077 0.4219  8.413e-09
#> a=2   0.4961 0.04732 0.4034 0.5889  1.019e-25
#> a=3   0.6348 0.02492 0.5860 0.6836 3.667e-143
#> 
#> Average Treatment Effect (constrast: 'a=1' - 'a=3'):
#> 
#>     Estimate Std.Err    2.5%   97.5%   P-value
#> ATE    -0.32 0.05923 -0.4361 -0.2039 6.577e-08
#> 
```
