// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <target/odesolver.hpp>
#include "targeted_types.h"

// [[Rcpp::export(name=".ode_solve")]]
arma::mat ode_solve(odeptr_t f, arma::mat input, arma::mat init,
                    arma::mat par ) {
  target::RK4 MyODE(*f);
  arma::mat y = MyODE.solve(input, init, par);
  return y;
}

// [[Rcpp::export(name=".ode_solve2")]]
arma::mat ode_solve2(Rcpp::Function f, arma::mat input, arma::mat init,
                     arma::mat par) {

  target::odefunc ff = [&f](arma::mat input, arma::mat x, arma::mat theta) {
    return Rcpp::as<arma::mat>(f(input,x,theta)); };

  target::RK4 MyODE(ff);
  arma::mat y = MyODE.solve(input, init, par);
  return y;
}
