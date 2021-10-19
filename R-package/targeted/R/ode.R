##' Define compiled code for ordinary differential equation.
##'
##' The model (\code{code}) should be specified as the body of of C++ function.
##' The following variables are defined bye default (see the argument \code{pname})
##' \itemize{
##' \item{dy}{Vector with derivatives, i.e. the rhs of the ODE (the result).}
##' \item{x}{Vector with the first element being the time, and the following
##' elements additional exogenous input variables,}
##' \item{y}{Vector with the dependent variable}
##' \item{p}{Parameter vector}
##' }
##' \eqn{y'(t) = f_{p}(x(t), y(t))}
##' All variables are treated as Armadillo (http://arma.sourceforge.net/) vectors/matrices.
##'
##' As an example consider the *Lorenz Equations*
##' \eqn{\frac{dx_{t}}{dt} = \sigma(y_{t}-x_{t})}
##' \eqn{\frac{dy_{t}}{dt} = x_{t}(\rho-z_{t})-y_{t}}
##' \eqn{\frac{dz_{t}}{dt} = x_{t}y_{t}-\beta z_{t}}
##'
##' We can specify this model as
##' \code{ode <- 'dy(0) = p(0)*(y(1)-y(0));
##'       dy(1) = y(0)*(p(1)-y(2));
##'       dy(2) = y(0)*y(1)-p(2)*y(2);'}
##' \code{dy <- specify_ode(ode)}
##'
##' As an example of model with exogenous inputs consider the following ODE:
##' \eqn{y'(t) = \beta_{0} + \beta_{1}y(t) + \beta_{2}y(t)x(t) + \beta_{3}x(t)\cdot t}
##' This could be specified as
##' \code{mod <- 'double t = x(0);
##'               dy = p(0) + p(1)*y + p(2)*x(1)*y + p(3)*x(1)*t;'}
##' \code{dy <- specify_ode(mod)}##'
##' @title Specify Ordinary Differential Equation (ODE)
##' @param code string with the body of the function definition (see details)
##' @param fname Optional name of the exported C++ function
##' @param pname Vector of variable names (results, inputs, states, parameters)
##' @return pointer (externalptr) to C++ function
##' @author Klaus Kähler Holst
##' @seealso solve_ode
##' @examples
##' ode <- 'dy(0) = p(0)*(y(1)-y(0));
##'         dy(1) = y(0)*(p(1)-y(2));
##'         dy(2) = y(0)*y(1)-p(2)*y(2);'
##' \donttest{  # Reduce test time
##' dy <- specify_ode(ode)}
##' tt <- seq(0, 100, length.out=2e4)
##' yy <- solve_ode(f, input=tt, init=c(1, 1, 1), par=c(10, 28, 8/3))
##' }
##' @export
specify_ode <- function(code, fname=NULL, pname=c("dy", "x", "y", "p")) {
  if (is.null(fname))
    fname <- paste0("dy_", digest::sha1(code))
  hd <- "// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(targeted)]]
#include <RcppArmadillo.h>
#include <targeted.h>
using namespace arma;
"
  fun <- paste0("arma::rowvec ", fname,
                "(const arma::rowvec &", pname[2], ",
const arma::rowvec &", pname[3], ",
const arma::rowvec &", pname[4], ") {
  arma::rowvec ", pname[1], "(", pname[3], ".n_elem, arma::fill::zeros);
")
  fptr <- paste0("
  return ", pname[1], ";
}
// [[Rcpp::export]]
odeptr_t make_dy() {
  return odeptr_t(new odefunc_t(")
  tl <- "));
}
"
  make_dy <- NULL # Avoid warning about missing global variable
  rcpp_code <- paste(hd, fun, code, fptr, fname, tl)
  res <- Rcpp::sourceCpp(code=rcpp_code)
  f <- make_dy()
  return(f)
}


##' Solve ODE with Runge-Kutta method (RK4)
##'
##' The external point should be created with the function \code{targeted::specify_ode}.
##' @title Solve ODE
##' @param ode_ptr pointer (externalptr) to C++ function
##' @param input Input matrix. 1st column specifies the time points
##' @param init Initial conditions
##' @param par Parameters defining the ODE (parsed to ode_ptr)
##' @seealso specify_ode
##' @return Matrix with solution
##' @author Klaus Kähler Holst
##' @export
##' @examples
##' example(specify_ode)
solve_ode <- function(ode_ptr, input, init, par=0) {
  .ode_solve(ode_ptr, cbind(input), rbind(init), rbind(par))
}
