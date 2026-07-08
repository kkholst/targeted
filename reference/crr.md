# Conditional Relative Risk estimation

Conditional Relative Risk estimation via Double Machine Learning

## Usage

``` r
crr(
  treatment,
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

d <- sim1(n = 2e3, seed = 1)
if (require("SuperLearner",quietly=TRUE)) {
  e <- crr(data=d,
   type = "dml2",
   treatment = a ~ v,
   response_model = learner_glm(y~ a*(x + v + I(v^2))),
   importance_model = learner_glm(D_ ~ v + I(v^2)),
   propensity_model = learner_glm(a ~ x + v + I(v^2), family=binomial),
           nfolds = 2)
  summary(e) # the true parameters are c(1,1)
}
#> crr(treatment = a ~ v, response_model = learner_glm(y ~ a * (x + 
#>     v + I(v^2))), propensity_model = learner_glm(a ~ x + v + 
#>     I(v^2), family = binomial), importance_model = learner_glm(D_ ~ 
#>     v + I(v^2)), data = d, nfolds = 2, type = "dml2")
#> 
#>    Estimate Std.Err    2.5% 97.5%   P-value
#> p1   1.1710 0.67906 -0.1599 2.502 8.462e-02
#> p2   0.9794 0.06828  0.8456 1.113 1.178e-46
```
