#pragma once

#include <RcppArmadillo.h>


using odefunc_t = std::function<arma::mat(arma::mat input,
                                          arma::mat x,
                                          arma::mat theta)>;

using odeptr_t =  Rcpp::XPtr<odefunc_t>;
