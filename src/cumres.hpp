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
    arma::vec t;    // Variable to order after
    arma::uvec ord; // Order
    arma::mat qt;   //  

    
    cumres(const arma::vec &r, const arma::mat &dr, const arma::mat &ic); //constructor    
    void reorder(const arma::vec &ord);
    arma::vec rnorm();
    arma::vec obs();
    arma::vec sample(arma::uvec idx=arma::uvec());
    arma::mat sample(unsigned R, arma::uvec idx=arma::uvec(), bool quantiles=true);
    
  };

  
}  // namespace target

