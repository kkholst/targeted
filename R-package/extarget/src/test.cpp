// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(targeted)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <target/odesolver.hpp>

arma::vec dy(const arma::vec &input,
             const arma::vec &x,
             const arma::vec &par) {
  return par(0) + par(1)*x;
}

// [[Rcpp::export(name="ode")]]
arma::mat odetest(arma::vec &t) {
  target::RK4 MyODE(dy);
  // arma::vec y0 = arma::zeros(1);
  // arma::vec par = { 1.0, 1.0 };
  // arma::vec y = MyODE.solve(t, y0, par);
  // return(y);
  return(t);

}
