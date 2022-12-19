/*!
  @file nb_interface.cpp
  @author Klaus K. Holst
  @copyright 2018-2021, Klaus Kähler Holst

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
Rcpp::List NB(arma::vec y, arma::mat x,
	      arma::uvec xlev, arma::vec ylev,
	      arma::vec weights, double laplacesmooth=1.0) {
  std::vector<target::raggedArray> res;
  res = target::nb(y, x, xlev, ylev, weights, laplacesmooth);
  return( Rcpp::wrap(res) );
}


// [[Rcpp::export(name=".predNB")]]
arma::mat predNB(arma::mat const &X,
                 Rcpp::List const &condprob,
                 Rcpp::List const &xord,
                 arma::uvec multinomial,
                 arma::vec prior,
                 double threshold=1E-3) {

  target::raggedArray condprob0;
  for (unsigned i=0; i<condprob.size(); i++) {
    condprob0.push_back(condprob[i]);
  }
  target::raggedArray xord0;
  for (unsigned i=0; i<xord.size(); i++) {
    xord0.push_back(xord[i]);
  }
  arma::mat res = target::prednb(X, condprob0, xord0,
                         multinomial, prior, threshold);
  return(res);
}
