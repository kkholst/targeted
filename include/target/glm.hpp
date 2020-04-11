/*!
  @file glm.hpp
  @author Klaus K. Holst
  @copyright 2018-2020, Klaus Kähler Holst

  @brief Utility functions for Generalized Linear Models

*/

#pragma once

#ifndef ARMA_R
#define MATHLIB_STANDALONE
#include <armadillo>
//#include "Rmath.h"
#endif
#if defined(ARMA_R)
#include <RcppArmadillo.h>
#endif
#include <cmath>
#include <complex>
#include <cfloat>     // precision of double (DBL_MIN)
#include <functional> // std::bind for using non-static member function as argument to free function

using cx_dbl  = std::complex<double>;
using cx_func = std::function<arma::cx_mat(arma::cx_vec theta)>;
using matlist = std::vector<arma::mat>;

namespace target {

  arma::mat expit(arma::mat x);
  arma::cx_mat expit(arma::cx_mat x);
  arma::vec softmax(arma::vec u);
  arma::mat softmax(arma::mat lp, bool ref, bool log);
  
  // template<typename T>
  // arma::Mat<T> expit(const arma::Mat<T> &x) {
  //   return 1.0/(1+exp(-x));
  // }
  arma::mat expit(arma::mat x);
  arma::cx_mat expit(arma::cx_mat x);

  class IID {
  public:
    arma::mat iid;
    arma::mat vcov;
    IID(): iid(arma::zeros(1, 1)), vcov(arma::zeros(1, 1)) {}
    IID(arma::mat score, arma::mat v): iid(score*v), vcov(v) {}
  };

  IID logistic_iid(const arma::vec &y,
		   const arma::vec &p,
		   const arma::mat &x,
		   const arma::vec &w);

  IID linear_iid(const arma::vec &y,
		 const arma::vec &p,
		 const arma::mat &x,
		 const arma::vec &w);

}  // namespace target
