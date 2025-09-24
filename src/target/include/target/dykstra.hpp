/*!
  @file dykstra.hpp
  @author Klaus K. Holst
  @copyright 2020-2025, Klaus Kähler Holst

  @brief Dykstra's alternating projection algorithm for
  least squares problems with inequality constraints
*/
#pragma once

#include "utils.hpp"

namespace target {

  class SignedWald {
    public:
      double sw;
      double pval;
      arma::vec sol;
  };

  SignedWald signedwald_sim(const arma::vec &par, const arma::mat &vcov,
                            const arma::vec &noninf,
                            const arma::vec &weights,
                            unsigned nsim_null = 1e4
                            );


// min ||x-u||^2 s.t. Au <= 0
  raggedArray lsdykstra(const arma::vec &x, const arma::mat &A,
                        double tol = 1e-7, unsigned iter_max = 500);

}  // namespace target
