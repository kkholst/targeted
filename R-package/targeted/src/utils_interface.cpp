/*!
  @file utils_interface.cpp
  @author Klaus K. Holst
  @copyright 2018-2020, Klaus Kähler Holst

  @brief R bindings for utility functions.

  The relevant bindings are created in \c RcppExports.cpp, \c RcppExports.h
  with \c Rcpp::compileAttributes()
*/

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include "utils_interface.h"
#include <target/utils.hpp>
#include <target/glm.hpp>


// [[Rcpp::export(name=".clusterid")]]
Rcpp::List clusterid(const arma::uvec &id) {
  arma::umat res = target::clusterid(id);
  return Rcpp::List::create(Rcpp::Named("size")=res.col(1),
			    Rcpp::Named("idx")=res.col(0)+1,
			    Rcpp::Named("n")=res.n_rows);
}

// [[Rcpp::export(name=".groupsum")]]
arma::mat groupsum(const arma::mat &x,
		   const arma::uvec &cluster,
		   bool reduce=true) {

  return target::groupsum(x, cluster, reduce);
}

// [[Rcpp::export(name=".softmax")]]
arma::mat softmax(arma::mat &lp, bool ref=true, bool log=false) {
  return target::softmax(lp, ref, log);
}
