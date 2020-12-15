/*!
  @file kalman.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Kalman filter

*/
#include <target/kalman.hpp>

////////////////////////////////////////////////////////////////////////////////

namespace target {

  using namespace arma;

  void KalmanFilter::UpdateData(const arma::mat H,
				const arma::mat Q,
				const arma::mat y) {
    this->H = H;
    this->Q = Q;
    this->y = y;
  }

  mat KalmanFilter::filter() {
    return H*Q;
  }


}  // namespace target
