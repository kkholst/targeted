/*!
  @file rinterface.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief R interface for the cumres class. 

  The relevant bindings are created in \c RcppExports.cpp, \c RcppExports.h
  after running \c Rcpp::compileAttributes()
*/

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(cpp11)]]

#include "cumres.hpp"
#include <RcppArmadillo.h>
#include <cmath>

// [[Rcpp::export]]
double SupTest(const arma::vec &x) {
  return target::SupTest(x);
}

// [[Rcpp::export]]
double L2Test(const arma::vec &x, const arma::vec &t) {
  return target::L2Test(x, t);
}

RCPP_MODULE(gofmod) {
    using namespace Rcpp;
    class_<target::cumres>("CumRes")
    // expose the constructor
      .constructor<arma::vec, 
		   arma::mat,
		   arma::mat>("Constructor")
      .field( "t", &target::cumres::t )
      .field( "ord", &target::cumres::ord )
      .field( "r", &target::cumres::r )
      .field( "qt", &target::cumres::qt )

      .method("samplestat", (arma::mat (target::cumres::*)(unsigned, arma::uvec, bool ) )( &target::cumres::sample),
      	       "sample process and return Sup and L2 statistic")
      .method("sample1", (arma::vec (target::cumres::*)(arma::uvec) )( &target::cumres::sample),
	       "sample process")      
      .method("obs",    &target::cumres::obs,   "Return observed process")      
      .method("rnorm",  &target::cumres::rnorm,   "Sample from Gaussian")
      .method("reorder",  &target::cumres::reorder,  "Order observations after input variable")
      ;
}

