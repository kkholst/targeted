/*!
  @file glm.hpp
  @author Klaus K. Holst
  @copyright 2019, Klaus Kähler Holst

  @brief Utility functions for Generalized Linear Models

*/

#ifndef SRC_GLM_H_
#define SRC_GLM_H_

#ifndef ARMA_R
#define MATHLIB_STANDALONE
#include <armadillo>
//#include "Rmath.h"
#endif
#if defined(ARMA_R)
#define ARMA_DONT_USE_OPENMP
#include <RcppArmadillo.h>
#endif
#include <cmath>
#include <complex>
#include <cfloat>     // precision of double (DBL_MIN)
#include <functional> // std::bind for using non-static member function as argument to free function


namespace target {
  using cx_dbl  = std::complex<double>;
  using cx_func = std::function<arma::cx_mat(arma::cx_vec theta)>;

  arma::mat deriv(cx_func f, arma::vec theta);
  
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

  // template<typename T>
  // arma::Mat<T> expit(const arma::Mat<T> &x) {
  //   return 1.0/(1+exp(-x));
  // }
  arma::mat expit(arma::mat x);
  arma::cx_mat expit(arma::cx_mat x);


}  // namespace target

#endif  // SRC_GLM_H_

