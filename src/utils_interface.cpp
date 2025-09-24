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
#include <target/pava.hpp>
#include <target/nondom.hpp>
#include <target/dykstra.hpp>

using namespace Rcpp;


// [[Rcpp::export(name=".clusterid")]]
List clusterid(const arma::uvec &id) {
  arma::umat res = target::clusterid(id);
  return List::create(Named("size")=res.col(1),
		      Named("idx")=res.col(0)+1,
		      Named("n")=res.n_rows);
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

// [[Rcpp::export(name = ".pava")]]
List pava(const arma::vec &y, const NumericVector &x, const NumericVector &weights) {
  arma::mat res = target::pava(y, x, weights);
  NumericVector idx = Rcpp::wrap(res.col(1)+1);
  idx.attr("dim") = R_NilValue;
  return Rcpp::List::create(Rcpp::Named("index")=idx,
			    Rcpp::Named("value")=res.col(0));

}

// [[Rcpp::export(name = ".nondom")]]
arma::mat nondom(const arma::mat &x) {
  return target::nondominated(x);
}

// [[Rcpp::export(name = ".dykstra")]]
List dykstra(const arma::vec &x, const arma::mat &A) {

  arma::vec w = arma::ones(x.n_elem);

  target::raggedArray res = target::lsdykstra(x, A);


  return Rcpp::List::create(Rcpp::Named("solution") = res[0],
                            Rcpp::Named("iter") = res[1]
                            );
}

// [[Rcpp::export(name = ".signedwald")]]
List signedwald(const arma::vec &par, const arma::mat &vcov,
             const arma::vec &noninf, const arma::vec &weights,
             unsigned nsim_null=1e4) {

  target::SignedWald res =
    target::signedwald_sim(par, vcov, noninf, weights, nsim_null);

  return Rcpp::List::create(Rcpp::Named("solution") = res.sol,
                            Rcpp::Named("test.statistic") = res.sw,
                            Rcpp::Named("pval") = res.pval
                            );
}
