// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <target/odesolver.hpp>
#include "targeted_types.h"

// [[Rcpp::export(name=".ode_solve")]]
arma::mat ode_solve(odeptr_t f,
                    arma::mat input,
                    arma::mat init,
                    arma::mat par ) {
   target::RK4 MyODE(*f);
   arma::mat y = MyODE.solve(input, init, par);
   return y;
}
