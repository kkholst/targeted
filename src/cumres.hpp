/*!
  @file cumres.hpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Generic class for cumulative residuals

*/

#pragma once
#include "utils.hpp"

namespace target {
 
  class cumres {
  public:
    unsigned n;
    arma::vec r;    // Residuals    
    arma::mat dr;   // Derivative of residuals wrt model parameters
    arma::mat eta;    // Cumulative derivative of residuals
    arma::mat ic;   // Influence curve
    arma::umat ord; // Stores order of observations to cumulate after
    arma::mat qt;   // Stores data for calculations of quantiles of residual processes

    
    cumres(const arma::vec &r, const arma::mat &dr, const arma::mat &ic); //constructor    
    void reorder(const arma::vec &ord);
    void reorder(const arma::mat &inp);
    arma::vec rnorm();
    arma::vec obs();
    arma::vec sample(arma::uvec idx=arma::uvec());
    arma::mat sample(unsigned R, arma::uvec idx=arma::uvec(), bool quantiles=true);
    
  };

  
}  // namespace target

