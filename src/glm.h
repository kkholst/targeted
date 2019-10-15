/*!
  @file glm.h
  @author Klaus K. Holst
  @copyright 2018, Klaus Kähler Holst

  @brief Utility functions for Generalized Linear Models

*/

#ifndef SRC_GLM_H_
#define SRC_GLM_H_

#ifndef RARMA
#define MATHLIB_STANDALONE
#include <armadillo>
#include "Rmath.h"
#endif
#if defined(RARMA)
#define ARMA_DONT_USE_OPENMP
#include <RcppArmadillo.h>
#endif
#include <complex>
typedef std::complex<double> Complex;

namespace glm {

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

  template<typename T>
  arma::Mat<T> expit(const arma::Mat<T> &x) { return 1.0/(1+exp(-x)); }

}  // namespace glm

#endif  // SRC_GLM_H_

