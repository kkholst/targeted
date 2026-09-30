# Specify Ordinary Differential Equation (ODE)

Define compiled code for ordinary differential equation.

## Usage

``` r
specify_ode(code, fname = NULL, pname = c("dy", "x", "y", "p"))
```

## Arguments

- code:

  string with the body of the function definition (see details)

- fname:

  Optional name of the exported C++ function

- pname:

  Vector of variable names (results, inputs, states, parameters)

## Value

pointer (externalptr) to C++ function

## Details

The model (`code`) should be specified as the body of of C++ function.
The following variables are defined bye default (see the argument
`pname`)

- dy:

  Vector with derivatives, i.e. the rhs of the ODE (the result).

- x:

  Vector with the first element being the time, and the following
  elements additional exogenous input variables,

- y:

  Vector with the dependent variable

- p:

  Parameter vector

\\y'(t) = f\_{p}(x(t), y(t))\\ All variables are treated as Armadillo
(http://arma.sourceforge.net/) vectors/matrices.

As an example consider the *Lorenz Equations* \\\frac{dx\_{t}}{dt} =
\sigma(y\_{t}-x\_{t})\\ \\\frac{dy\_{t}}{dt} =
x\_{t}(\rho-z\_{t})-y\_{t}\\ \\\frac{dz\_{t}}{dt} = x\_{t}y\_{t}-\beta
z\_{t}\\

We can specify this model as
`ode <- 'dy(0) = p(0)*(y(1)-y(0)); dy(1) = y(0)*(p(1)-y(2)); dy(2) = y(0)*y(1)-p(2)*y(2);'`
`dy <- specify_ode(ode)`

As an example of model with exogenous inputs consider the following ODE:
\\y'(t) = \beta\_{0} + \beta\_{1}y(t) + \beta\_{2}y(t)x(t) +
\beta\_{3}x(t)\cdot t\\ This could be specified as
`mod <- 'double t = x(0); dy = p(0) + p(1)*y + p(2)*x(1)*y + p(3)*x(1)*t;'`
`dy <- specify_ode(mod)`

## See also

solve_ode

## Author

Klaus Kähler Holst

## Examples

``` r
ode <- paste0(
  "dy(0) = p(0)*(y(1)-y(0));",
  "dy(1) = y(0)*(p(1)-y(2));",
  "dy(2) = y(0)*y(1)-p(2)*y(2);", collapse="\n"
)
 # Reduce test time
dy <- specify_ode(ode)
tt <- seq(0, 100, length.out=2e4)
yy <- solve_ode(dy, input=tt, init=c(1, 1, 1), par=c(10, 28, 8/3))
```
