
/*!
  @file kalman.hpp
  @author Klaus K. Holst
  @copyright 2020-2021, Klaus

  @brief Kalman filter

*/

#pragma once

#include "utils.hpp"

namespace target {

  class LinearGaussian {
  private:
    arma::cube Y;  // observations
    arma::mat T;  // transition matrix
    arma::cube Z;  // emission/design matrix
    arma::mat H;  // observation error variance
    arma::mat Q;  // state error variance
    arma::vec a0;  // initial state mean
    arma::mat P0;  // initial state variance
    arma::mat K;  // regression matrix observation eq.
    arma::mat L;  // regression matrix state eq.
	arma::cube x; // exogenous covariates
	arma::vec c; // drift term observation eq.
	arma::vec d; // drift term state eq.

  public:
    unsigned n; // Number of observations
    unsigned p; // Length of response vector
    unsigned m; // Length of state vector
    double llh; // log likelihood value

// cubes for filter output
    arma::cube a_t, P_t; // mean and variance of predictive distribution : E[alpha_t| y_{1:t-1}], Var[alpha_t| y_{1:t-1}]
    arma::cube a_tt, P_tt; // mean and variance of filter distribution : E[alpha_t| y_{1:t}], Var[alpha_t| y_{1:t}]
    arma::cube v_t, K_t; // v_t = y_t - E[y_t|y_{1:t-1}] and Kalman gain K_t= T_t * P_t * Z_t^\top * F_t^{-1};
    arma::cube F_t, F_t_inv; // Var(v_t|y_{1:t-1}) and its inverse: F_t_inv = F_t^{-1}

// cubes for smoother output
    arma::cube a_tn, P_tn; // E[alpha_t| y_{1:n}], Var[alpha_t| y_{1:n}]
    arma::cube J_t, P_tt1n; // P_t1t2n = Cov[alpha_{t},alpha_{t-1}| y_{1:n}]. J_t is an auxiliary matrix
    arma::mat a_0n, P_0n; // E[alpha_0| y_{1:n}], Var[alpha_0| y_{1:n}]


    LinearGaussian() {}  // Empty constructor

     void UpdateData(const arma::cube Y,
            const arma::mat T,
		    const arma::cube Z,
            const arma::mat H,
		    const arma::mat Q,
		    const arma::vec a0,
		    const arma::mat P0,
		    const arma::mat K,
		    const arma::mat L,
		    const arma::cube x,
		    const arma::vec c,
		    const arma::vec d);

    LinearGaussian(const arma::cube Y,
            const arma::mat T,
		    const arma::cube Z,
            const arma::mat H,
		    const arma::mat Q,
		    const arma::vec a0,
		    const arma::mat P0,
		    const arma::mat K,
		    const arma::mat L,
		    const arma::cube x,
		    const arma::vec c,
		    const arma::vec d){
		    UpdateData(Y,T,Z,H,Q,a0,P0, L, K, x, c, d);
		    }

    void filter(); // Kalman filter
    void smoother(); // fixed-interval smoother

  };  // class LinearGaussian

}  // namespace target
