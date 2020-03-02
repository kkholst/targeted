/*!
  @file utils.hpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Various utility functions and constants

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


namespace target {  
  using cx_dbl  = std::complex<double>;
  using cx_func = std::function<arma::cx_mat(arma::cx_vec theta)>;
  
  arma::mat deriv(cx_func f, arma::vec theta);

  arma::umat clusterid(const arma::uvec &id);  
  arma::mat groupsum(const arma::mat &x, const arma::uvec &cluster, bool reduce);  
  
  double SupTest(const arma::vec &D);
  double L2Test(const arma::vec &D, const arma::vec &t);  
  double CramerVonMises(const arma::vec &x, const arma::vec &G);

  extern arma::mat const EmptyMat;
  extern arma::vec const EmptyVec;  
  
  extern const char* COL_RESET;
  extern const char* COL_DEF;
  extern const char* BLACK;
  extern const char* RED;
  extern const char* MAGENTA;
  extern const char* YELLOW;
  extern const char* GREEN;
  extern const char* BLUE;
  extern const char* CYAN;
  extern const char* WHITE;
  extern const char* GRAY;
  extern const char* LRED;
  extern const char* LGREEN;
  extern const char* LYELLOW;
  extern const char* LBLUE;
  extern const char* LMAGENTA;
  extern const char* LCYAN;
  extern const char* LWHITE;
  
}  // namespace target
