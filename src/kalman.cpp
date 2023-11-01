/*!
  @file kalman.cpp
  @author Klaus K. Holst
  @copyright 2020-2021, Klaus Kähler Holst

  @brief Kalman filter

*/
#include <target/kalman.hpp>
#include <cmath>

////////////////////////////////////////////////////////////////////////////////

namespace target {

  using namespace arma;

   void LinearGaussian::UpdateData(const arma::cube Y,
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
		    const arma::vec d) {

    this->Y = Y;
    this->T = T;
    this->Z = Z;
    this->H = H;
    this->Q = Q;
    this->a0 = a0;
    this->P0 = P0;
    this->L = L;
    this->K = K;
    this->x = x;
    this->c = c;
    this->d = d;

//  assign dimensions of the state space model
    this-> m = T.n_rows; // size of latent vector
    this-> p = Y.n_rows; // size of response vector
    this-> n = Y.n_slices; // length of time series
  }

  void LinearGaussian::filter() {
     cube a_tt(m,1,n,fill::zeros), a_t(m,1,n,fill::zeros);
     cube P_tt(m,m,n,fill::zeros), P_t(m,m,n,fill::zeros);
     cube F_t(p,p,n,fill::zeros), F_t_inv(p,p,n,fill::zeros);
     cube v_t(p,1,n,fill::zeros), K_t(m,p,n,fill::zeros);

     a_t.slice(0) = T * a0 + L * x.slice(0); // E[alpha_1|y_0] = E[alpha_1]
     P_t.slice(0) = T * P0 * T.t() + Q; // Var[alpha_1]

     llh = -0.5 * n * p * log(2*M_PI); // log likelihood value
     for (int i = 0; i < n; i++) { // Kalman filter
        v_t.slice(i) = Y.slice(i) - Z.slice(i) * a_t.slice(i) - K * x.slice(i) - c; // y_t - E[y_t|y_{1:t-1}]
        F_t.slice(i) = Z.slice(i) * P_t.slice(i) * Z.slice(i).t() + H;  // Var(v_t|y_{1:t-1})
        F_t_inv.slice(i) = pinv(F_t.slice(i));

        K_t.slice(i) = T * P_t.slice(i) * Z.slice(i).t() * F_t_inv.slice(i); // Kalman gain
        llh -= 0.5 * log(det(F_t.slice(i)));
        llh -= 0.5 * as_scalar(v_t.slice(i).t() * F_t_inv.slice(i) * v_t.slice(i));

        a_tt.slice(i) = a_t.slice(i) + P_t.slice(i) * Z.slice(i).t() * F_t_inv.slice(i) * v_t.slice(i); // mean of filter distribution: E[alpha_t|y_{1:t}]
        P_tt.slice(i) = P_t.slice(i) - P_t.slice(i) * Z.slice(i).t() * F_t_inv.slice(i) * Z.slice(i) * P_t.slice(i); // variance of filter distribution: Var[alpha_t|y_{1:t}]

        if (i < n-1){ // avoid calculating predictive distribution mean (E[alpha_{n+1}|y_{1:n}]) and variance because no matching y_{n+1} exists in the time series
        a_t.slice(i+1) = T*a_tt.slice(i) + L * x.slice(i+1) + d; // E[alpha_{t+1}|y_{1:t}]
        P_t.slice(i+1) = T*P_tt.slice(i)*T.t() + Q; // Var[alpha_{t+1}|y_{1:t}]
        }
    } // for loop

    this -> a_t = a_t;
    this -> P_t = P_t;
    this -> a_tt = a_tt;
    this -> P_tt = P_tt;
    this -> K_t = K_t;
    this -> F_t = F_t;
    this -> F_t_inv = F_t_inv;
    this -> v_t = v_t;
  } // LinearGaussian::filter()


  void LinearGaussian::smoother(){ // the smoothing algorithm is shown in see Shumway&Stoffer(1982): an approach to time series smoothing and forecasting)
  // initialization
  cube a_tn(m,1,n,fill::zeros), J_t(m,m,n,fill::zeros);
  cube P_tn(m,m,n,fill::zeros), P_tt1n(m,m,n,fill::zeros);

//  set values in a_tn, J_t and P_tn for t=n
  a_tn.slice(n-1) = a_tt.slice(n-1); // E[alpha_n|y_{1:n}] is the mean of the filter distribution for t=n
  P_tn.slice(n-1) = P_tt.slice(n-1); // Var[alpha_n|y_{1:n}] is the variance of the filter distribution for t=n
  J_t.slice(n-1) = P_tt.slice(n-1) * T.t() * pinv(P_t.slice(n-1)); // auxiliary matrix for t=n

//  1. backward pass through data to compute E[alpha_t|y_{1:n}] and Var[alpha_t|y_{1:n}]
  for (int i = n-1; i  > 0; i--) {
    J_t.slice(i-1) = P_tt.slice(i-1) * T.t() * pinv(P_t.slice(i));
    a_tn.slice(i-1) = a_tt.slice(i-1) + J_t.slice(i-1) * (a_tn.slice(i) - a_t.slice(i)); // E[alpha_t|y_{1:n}]
    P_tn.slice(i-1) = P_tt.slice(i-1) + J_t.slice(i-1) * (P_tn.slice(i) - P_t.slice(i)) * J_t.slice(i-1).t(); // Var[alpha_t|y_{1:n}]
    } // for loop

//  calculate E[alpha_0|y_{1:n}]=a_0n and Var[alpha_0|y_{1:n}]=P_0n
  mat J0 = P0 * T.t() * pinv(P_t.slice(0));
  mat a_0n = a0 + J0 * (a_tn.slice(0) - a_t.slice(0)); // E[alpha_0|y_{1:n}]
  mat P_0n = P0 + J0 * (P_tn.slice(0) - P_t.slice(0)) * J0.t(); // Var[alpha_0|y_{1:n}]

//  compute Cov(alpha_n, alpha_{n-1}|y_{1:n})
  P_tt1n.slice(n-1) = (eye(m,m) - T * P_t.slice(n-1) * Z.slice(n-1).t() * F_t_inv.slice(n-1) * Z.slice(n-1)) * T * P_tt.slice(n-2);

// 2. backward pass through data to compute Cov(alpha_t, alpha_{t-1}| y_{1:n})
  for (int i = n-1; i  > 1; i--) {
    P_tt1n.slice(i-1) = P_tt.slice(i-1) * J_t.slice(i-2).t();
    P_tt1n.slice(i-1) += J_t.slice(i-1) * (P_tt1n.slice(i) - T * P_tt.slice(i-1)) * J_t.slice(i-2).t();
    } // for loop

  // calculate Cov(alpha_1, alpha_0|y_{1:n})
  P_tt1n.slice(0) = P_tt.slice(0) * J0.t();
  P_tt1n.slice(0) += J_t.slice(0) * (P_tt1n.slice(1) - T * P_tt.slice(0)) * J0.t();

  this -> a_tn = a_tn;
  this -> P_tn = P_tn;
  this -> a_0n = a_0n;
  this -> P_0n = P_0n;
  this -> P_tt1n = P_tt1n;
  } // LinearGaussian::smoother()


}  // namespace target
