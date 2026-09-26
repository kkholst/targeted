# Find non-dominated points of a set

Find the non-dominated point of a set (minima of a point set).

## Usage

``` r
nondom(x, ...)
```

## Arguments

- x:

  matrix

- ...:

  additional arguments to lower level functions

## Value

matrix

## Details

A point x dominates y if it is never worse and at least in one case
strictly better. Formally, let f_i denote the ith coordinate of the
condition (objective) function, then for all i: f_i(x)\<=f_i(y) and
there exists j: f_j(x)\<f_j(y).

Based on the algorithm of Kung et al. 1975.

## Author

Klaus Kähler Holst

## Examples

``` r
rbind(
  c(1.0, 0.5),
  c(0.0, 1.0),
  c(1.0, 0.0),
  c(0.5, 1.0),
  c(1.0, 1.0),
  c(0.8, 0.8)) |> nondom()
#>      [,1] [,2]
#> [1,]  0.0  1.0
#> [2,]  0.8  0.8
#> [3,]  1.0  0.5
```
