/*!
  @file cumres.hpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Generic class for cumulative residuals

*/

#pragma once
#include "utils.hpp"

namespace target {

  /**
   * The <code>cumres</code> class provides a data structure for
   * calculating goodness-of-fit statistics based on aggregation of
   * residuals (cumulative residuals) of a statistical model.
 */
  class cumres {
  public:
    unsigned   n;    ///< Sample size
    arma::vec  r;    ///< Residuals
    arma::umat ord;  ///< Stores order of observations to cumulate after
    arma::mat  dr;   ///< Derivative of residuals wrt model parameters
    arma::mat  ic;   ///< Influence curve
    arma::mat  inp;  ///< Variable to order residuals after
    arma::vec  b;    ///< Bandwidth of moving average
    arma::mat  qt;   ///< Stores data for calculations of quantiles
    arma::mat  eta;    ///< Cumulative derivative of residuals

    cumres(const arma::vec &r,
	   const arma::mat &dr,
	   const arma::mat &ic); ///< Constructor
    void order(const arma::mat &inp, arma::vec b=arma::vec()); ///< Set variables to order after and bandwidth
    arma::vec rnorm(); ///< Draw n samples from standard normal distribution
    arma::mat obs(); ///< Calculate observed cumulative residual process
    arma::mat sample(const arma::umat &idx = arma::umat()); ///< Simulate one process under the null hypothesis of a correctly specified model
    arma::mat sample(unsigned R,
		     const arma::umat &idx = arma::umat(),
		     bool quantiles = true); ///< Sample R processes and calculate test statistics under the null (Suprememum and L2 statistics)
  };


}  // namespace target
