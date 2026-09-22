# Create a list from all combination of input variables

Similar to `expand.grid` function, this function creates all
combinations of the input arguments but returns the result as a list.

## Usage

``` r
expand.list(..., INPUT = NULL, envir = NULL)
```

## Arguments

- ...:

  input variables

- INPUT:

  optional list of variables

- envir:

  environment environment to evalute formulas in

## Value

list

## Author

Klaus Kähler Holst

## Examples

``` r
expand.list(x = 2:4, z = c("a", "b"))
#> [[1]]
#> [[1]]$x
#> [1] 2
#> 
#> [[1]]$z
#> [1] "a"
#> 
#> 
#> [[2]]
#> [[2]]$x
#> [1] 3
#> 
#> [[2]]$z
#> [1] "a"
#> 
#> 
#> [[3]]
#> [[3]]$x
#> [1] 4
#> 
#> [[3]]$z
#> [1] "a"
#> 
#> 
#> [[4]]
#> [[4]]$x
#> [1] 2
#> 
#> [[4]]$z
#> [1] "b"
#> 
#> 
#> [[5]]
#> [[5]]$x
#> [1] 3
#> 
#> [[5]]$z
#> [1] "b"
#> 
#> 
#> [[6]]
#> [[6]]$x
#> [1] 4
#> 
#> [[6]]$z
#> [1] "b"
#> 
#> 
```
