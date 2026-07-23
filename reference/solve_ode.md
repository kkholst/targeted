# Solve ODE

Solve ODE with Runge-Kutta method (RK4)

## Usage

``` r
solve_ode(ode_ptr, input, init, par = 0)
```

## Arguments

- ode_ptr:

  pointer (externalptr) to C++ function or an R function

- input:

  Input matrix. 1st column specifies the time points

- init:

  Initial conditions

- par:

  Parameters defining the ODE (parsed to ode_ptr)

## Value

Matrix with solution

## Details

The external point should be created with the function
[`targeted::specify_ode`](specify_ode.md).

## See also

specify_ode

## Author

Klaus Kähler Holst

## Examples

``` r
example(specify_ode)
#> 
#> spcfy_> ode <- paste0(
#> spcfy_+   "dy(0) = p(0)*(y(1)-y(0));",
#> spcfy_+   "dy(1) = y(0)*(p(1)-y(2));",
#> spcfy_+   "dy(2) = y(0)*y(1)-p(2)*y(2);", collapse="\n"
#> spcfy_+ )
#> 
#> spcfy_> ## No test: 
#> spcfy_>  # Reduce test time
#> spcfy_> dy <- specify_ode(ode)
#> 
#> spcfy_> tt <- seq(0, 100, length.out=2e4)
#> 
#> spcfy_> yy <- solve_ode(dy, input=tt, init=c(1, 1, 1), par=c(10, 28, 8/3))
#> 
#> spcfy_> ## End(No test)
#> spcfy_> 
#> spcfy_> 
#> spcfy_> 
```
