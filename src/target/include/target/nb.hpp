/*!
  @file nb.hpp
  @author Klaus K. Holst
  @copyright 2020-2021, Klaus Kähler Holst

  @brief Weighted Naive Bayes

*/

#pragma once
#include <vector>
#include "utils.hpp"

namespace target {

  using raggedArray = std::vector<arma::vec>;

  raggedArray pcond(const arma::uvec   &idx,
		    const arma::mat    &x,
		    const arma::uvec   &xlev,
		    const arma::vec    &weights,
		    double             laplacesmooth);

  std::vector<raggedArray> nb(arma::vec  y,
			      arma::mat  x,
			      arma::uvec xlev = arma::uvec(),
			      arma::vec  ylev = arma::vec(),
			      arma::vec  weights = arma::vec(),
			      double     laplacesmooth = 1.0);

  arma::mat prednb(arma::mat const &X,
                   raggedArray const &condprob,
                   raggedArray const &xord,
                   arma::uvec multinomial,
                   arma::vec  prior = arma::vec(),
                   double     threshold = 1E-3);

}  // namespace target
