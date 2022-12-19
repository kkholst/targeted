// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <target/kalman.hpp>
#include <target/utils.hpp>

using target::LinearGaussian;
using namespace Rcpp;

RCPP_MODULE(ssmodel) {
    class_<LinearGaussian>("LinearGaussian")
      .constructor<arma::cube,  // Y
                   arma::mat,   // T
                   arma::cube,  // Z
                   arma::mat,   // H
                   arma::mat,   // Q
                   arma::vec,   // a0
                   arma::mat    // P0
                   >("Constructor")
      .field_readonly("loglik", &LinearGaussian::llh, "log likelihood")
      .field_readonly("a_t", &LinearGaussian::a_t, "E(y_t| y1,...,y_{t-1})")
      .field_readonly("F_t", &LinearGaussian::F_t, "Var(y_t| y1,...,y_{t-1})")
      .method("update", &LinearGaussian::UpdateData,  "Update data")
      .method("filter", &LinearGaussian::filter,  "Kalman Filter");
}
