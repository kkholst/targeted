# Identify Stratification Variables

This is a special function that identifies stratification variables when
they appear on the right hand side of a formula.

## Usage

``` r
stratify(..., na.group = FALSE, shortlabel, sep = ", ")
```

## Arguments

- ...:

  any number of variables. All must be the same length.

- na.group:

  a logical variable, if `TRUE`, then missing values are treated as a
  distinct level of each variable.

- shortlabel:

  if `TRUE` omit variable names from resulting factor labels. The
  default action is to omit the names if all of the arguments are
  factors, and none of them was named.

- sep:

  the character used to separate groups, in the created label

## Value

a new factor, whose levels are all possible combinations of the factors
supplied as arguments.

## Details

When used outside of a `coxph` formula the result of the function is
essentially identical to the `interaction` function, though the labels
from `strata` are often more verbose.

## See also

[survival::strata](https://rdrr.io/pkg/survival/man/strata.html),
[learner_stratify](learner_stratify.md),
[interaction](https://rdrr.io/r/base/interaction.html)

## Examples

``` r
a <- factor(rep(1:3, 4), labels=c("low", "medium", "high"))
b <- factor(rep(1:4, 3))
levels(stratify(b))
#> [1] "1" "2" "3" "4"
levels(stratify(a, b, shortlabel=TRUE))
#>  [1] "low, 1"    "low, 2"    "low, 3"    "low, 4"    "medium, 1" "medium, 2"
#>  [7] "medium, 3" "medium, 4" "high, 1"   "high, 2"   "high, 3"   "high, 4"  
```
