# Construct learners from a grid of parameters

Construct learners from a grid of parameters

## Usage

``` r
learner_expand_grid(fun, args, names = TRUE, params = FALSE)
```

## Arguments

- fun:

  (function) A function that returns a [learner](learner.md).

- args:

  (list) Parameters that generate a grid of parameters with
  [expand.list](expand.list.md), where the set of parameters are then
  passed on to `fun`.

- names:

  (logical or character) If FALSE, then return a list without names. If
  TRUE, a named list is returned (see details).

- params:

  (logical) If FALSE, then no information about the parameters defined
  by `args` are added to the names of the returned list.

## Value

list

## Examples

``` r
lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3))
)
lrs # use info of constructed learner as names
#> $`xgboost reg:squarederror`
#> ────────── learner object ──────────
#> xgboost reg:squarederror 
#> 
#> Estimate arguments: max_depth=2, learning_rate=1, nrounds=2, subsample=1, reg_lambda=1, objective=reg:squarederror, eta=0.2 
#> Predict arguments:   
#> Formula: Sepal.Length ~ . <environment: 0x55813c7c7f70> 
#> 
#> $`xgboost reg:squarederror.1`
#> ────────── learner object ──────────
#> xgboost reg:squarederror 
#> 
#> Estimate arguments: max_depth=2, learning_rate=1, nrounds=2, subsample=1, reg_lambda=1, objective=reg:squarederror, eta=0.5 
#> Predict arguments:   
#> Formula: Sepal.Length ~ . <environment: 0x55813c7c7f70> 
#> 
#> $`xgboost reg:squarederror.2`
#> ────────── learner object ──────────
#> xgboost reg:squarederror 
#> 
#> Estimate arguments: max_depth=2, learning_rate=1, nrounds=2, subsample=1, reg_lambda=1, objective=reg:squarederror, eta=0.3 
#> Predict arguments:   
#> Formula: Sepal.Length ~ . <environment: 0x55813c7c7f70> 
#> 

lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
  names = "xgboost"
)
names(lrs) # use xgboost instead of info field for names
#> [1] "xgboost"   "xgboost.1" "xgboost.2"

lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
  names = "xgboost",
  params = TRUE
)
names(lrs) # also add parameters to names
#> [1] "xgboost:Sepal.Length ~ .:0.2" "xgboost:Sepal.Length ~ .:0.5"
#> [3] "xgboost:Sepal.Length ~ .:0.3"

lrs <- learner_expand_grid(
  learner_xgboost,
  list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
  names = FALSE
)
names(lrs) # unnamed list since names = FALSE
#> NULL
```
