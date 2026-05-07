# Conditional Relative Risk estimation

Conditional average treatment effect estimation via Double Machine
Learning

## Usage

``` r
cate_link(
  treatment,
  link = "identity",
  response_model,
  propensity_model,
  importance_model,
  contrast = c(1, 0),
  data,
  nfolds = 5,
  type = "dml1",
  ...
)
```

## Arguments

- treatment:

  formula specifying treatment and variables to condition on

- link:

  Link function

- response_model:

  SL object

- propensity_model:

  SL object

- importance_model:

  SL object

- contrast:

  treatment contrast (default 1 vs 0)

- data:

  data.frame

- nfolds:

  Number of folds

- type:

  'dml1' or 'dml2'

- ...:

  additional arguments to SuperLearner

## Value

cate.targeted object

## Author

Klaus Kähler Holst & Andreas Nordland

## Examples

``` r
# Example 1:
sim1 <- function(n=1e4,
                 seed=NULL,
                 return_model=FALSE, ...){
suppressPackageStartupMessages(require("lava"))
if (!is.null(seed)) set.seed(seed)
m <- lava::lvm()
distribution(m, ~x) <- gaussian.lvm()
distribution(m, ~v) <- gaussian.lvm(mean = 10)
distribution(m, ~a) <- binomial.lvm("logit")
regression(m, "a") <- function(v, x){.1*v + x}
distribution(m, "y") <- gaussian.lvm()
regression(m, "y") <- function(a, v, x){v+x+a*x+a*v*v}
if (return_model) return(m)
lava::sim(m, n = n)
}

if (require("SuperLearner",quietly=TRUE)) {
  d <- sim1(n = 1e3, seed = 1)
  e <- cate_link(data=d,
           type = "dml2",
           treatment = a ~ v,
           response_model = y~ a*(x + v + I(v^2)),
           importance_model = SL(D_ ~ v + I(v^2)),
           nfolds = 10)
  summary(e) # the true parameters are c(1,1)
}
#> Loaded gam 1.22-7
#> Super Learner
#> Version: 2.0-40
#> Package created on 2025-12-14
#> cate_link(treatment = a ~ v, response_model = y ~ a * (x + v + 
#>     I(v^2)), importance_model = SL(D_ ~ v + I(v^2)), data = d, 
#>     nfolds = 10, type = "dml2")
#> 
#>             Estimate Std.Err    2.5%  97.5% P-value
#> (Intercept)   -99.33  1.3531 -101.98 -96.68       0
#> v              20.03  0.1356   19.77  20.30       0
#> 
#> Average Treatment Effect:
#>                     Estimate Std.Err   2.5%  97.5% P-value
#> [(Intercept)] - [v]   -119.4   1.488 -122.3 -116.4       0
#> ────────────────────────────────────────────────────────────
#> Null Hypothesis: 
#>   [(Intercept)] - [v] = 0 
```
