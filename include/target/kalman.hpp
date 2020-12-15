
/*!
  @file kalman.hpp
  @author Klaus K. Holst
  @copyright 2020, Klaus

  @brief Kalman filter

*/

#pragma once

#include "utils.hpp"

namespace target {

  class KalmanFilter {
  private:
    arma::mat H;  // observation error variance
    arma::mat Q;  // state error variance
    arma::vec y;  // observations

  public:
    unsigned n;   // Number of observations

    KalmanFilter() {}  // Empty constructor

    void UpdateData(const arma::mat H,
		    const arma::mat Q,
		    const arma::mat y);

    KalmanFilter(const arma::mat H,
		 const arma::mat Q,
		 const arma::mat y): H(H), Q(Q), y(y) {}

    arma::mat filter();


  };  // class KalmanFilter

}  // namespace target
