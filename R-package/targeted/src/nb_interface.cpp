/*!
  @file nb_interface.cpp
  @author Klaus K. Holst
  @copyright 2018-2020, Klaus Kähler Holst

  @brief R interface for the Weighted Naive Bayes Classifier.

  The relevant bindings are created in \c RcppExports.cpp, \c RcppExports.h
  with \c Rcpp::compileAttributes()
*/

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <vector>
#include <target/nb.hpp>


// [[Rcpp::export(name=".NB")]]
Rcpp::List NB(arma::vec y, arma::vec x,
	      arma::uvec xlev, arma::vec ylev,
	      arma::vec weights, double laplacesmooth=1.0) {
  std::vector<target::raggedArray> res;
  res = target::nb(y, x, xlev, ylev, weights, laplacesmooth);
  return( Rcpp::wrap(res) );
}
