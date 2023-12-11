##' Define compiled code for ordinary differential equation.
##'
##' The model (\code{code}) should be specified as the body of of C++ function.
##' The following variables are defined bye default (see the argument \code{pname})
##' \describe{
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
##' @param ode_ptr pointer (externalptr) to C++ function or an R function
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
  if (is.function(ode_ptr)) {
    return(.ode_solve2(ode_ptr, cbind(input), rbind(init), rbind(par)))
  }
  .ode_solve(ode_ptr, cbind(input), rbind(init), rbind(par))
}

## Dormand-Prince method. Adaptive step-size, see E. Hairer, S. P. Norsett G.
## Wanner, "Solving Ordinary Differential Equations I: Nonstiff Problems", Sec.
## II.
solve_ode_adaptive <- function(f, y0, t0, t1,  h0=0.1,
                               control=list()) {

  control0 <- list(hmax=h0, hmin=1e-20,
                   fac=0.9, fac_max=5, fac_min=1/5,
                   atol=1e-6, rtol=1e-6)
  control0[names(control)] <- control
  control <- control0

  # Adaptive step size
  cs <- c(0, 1/5, 3/10, 4/5, 8/9, 1, 1)
  as <- matrix(0, 7,7)
  as[,1] <- c(0,1/5, 3/40, 44/45, 19327/6561, 9017/3168, 35/384)
  as[,2] <- c(0,0, 9/40, -56/15, -25360/2187, -355/33, 0)
  as[,3] <- c(0,0,0, 32/9, 64448/6561, 46732/5247, 500/1113)
  as[,4] <- c(0,0,0,0, -212/729, 49/176, 125/192)
  as[,5] <- c(0,0,0,0,0, -5103/18656, -2187/6784)
  as[,6] <- c(0, 0, 0, 0, 0, 0, 11 / 84)
  bs <-  rbind(as[7,],
               c(5179/57600, 0, 7571/16695, 393/640, -92097/339200, 187/2100, 1/40))
  q <- nrow(as) - 1

  intstep <- function(t, h, y) {
    k <- matrix(0, nrow = q + 1, ncol = length(y0))
    for (i in seq(nrow(k))) {
      yval <- y
      for (j in seq_along(i-1))
        yval <- yval + as[i,j]*k[j,]
      k[i,] <- h*f(t+cs[i]*h, yval)
    }
    # Estimate y(x+h) as weighted average of the q increments
    if (identical(as[,q+1], bs[1,])) {
      y1 <- yval
    } else {
      y1 <- y
      for (j in seq_len(q+1))
        y1 <- y1 + bs[1,j]*k[j,]
    }
    ## Estimate y(x+h) as weighted average of the q+1 increments
    y2 <- y
    for (j in seq_len(q+1))
        y2 <- y2 + bs[2,j]*k[j,]

    return(list(y1, y2))
  }

  step <- function(t, h, y, fac_max) {
    accept <- FALSE
    while (!accept) {
      teval <- t + h
      step <- intstep(t, h, y)
      y1 <- step[[1]]
      y2 <- step[[2]]
      d <- abs(y2-y1)
      sc <- control$atol + control$rtol*pmax(abs(y1), abs(y2))
      err <- mean((d/sc)^2)^.5 ## err ~ C*h^(q+1)
      if (err<=1) { # accepts
        if (err<1e-30) {
          fac <- fac_max
        }
        accept <- TRUE
      }
      fac <- min(fac_max, max(control$fac_min, control$fac*(1/err)^(1/(q+1))))
      h <- min(control$hmax, h*fac)
      if (accept) {
        fac_max <- control$fac_max
      } else {
        fac_max <- 1
      }
    }
    return(structure(c(teval, y2), h=h, fac_max=fac_max))
  }

  t <- t0
  h <- max(h0,1e-30)
  y <- y0
  r0 <- c(t, y)
  fac <- control$fac_max
  while (t<t1) {
    r1 <- step(t, h, y, fac)
    r0 <- rbind(r0, r1)
    t <- r1[1]
    h <- min(t1-t, attr(r1, "h"))
    fac <- attr(r1, "fac_max")
    y <- r1[-1]
  }
  return(r0)
}
