#include <pybind11/stl.h>
#include <target/kalman.hpp>
#include "armapy.hpp"


namespace py = pybind11;
class LinearGaussianPy : public target::LinearGaussian {

public:
  LinearGaussianPy(pyarray &Y_arr, pyarray &T_arr, pyarray &Z_arr, pyarray &H_arr, pyarray &Q_arr, pyarray &a0_arr, pyarray &P0_arr,
                    pyarray &K_arr, pyarray &L_arr, pyarray &x_arr, pyarray &c_arr, pyarray &d_arr) {
    arma::cube Y = pycube(Y_arr);
    arma::mat T = pymat(T_arr);
    arma::cube Z = pycube(Z_arr);
    arma::mat H = pymat(H_arr);
    arma::mat Q = pymat(Q_arr);
    arma::vec a0 = pymat(a0_arr);
    arma::mat P0 = pymat(P0_arr);
    arma::mat K = pymat(K_arr);
    arma::mat L = pymat(L_arr);
    arma::cube x = pycube(x_arr);
    arma::vec c = pymat(c_arr);
    arma::vec d = pymat(d_arr);
    LinearGaussian::UpdateData(Y, T, Z, H, Q, a0, P0, K, L, x, c, d);
  }
    double llh; // log likelihood value
    py::dict filter_estimates, smoother_estimates; //  dictionaries to store filter and smoother outputs

    void filter() {
    LinearGaussian::filter();
    llh = LinearGaussian::llh;

    filter_estimates[py::str("a_t")] = cubepy(LinearGaussian::a_t);
    filter_estimates[py::str("P_t")] = cubepy(LinearGaussian::P_t);
    filter_estimates[py::str("a_tt")] = cubepy(LinearGaussian::a_tt);
    filter_estimates[py::str("P_tt")] = cubepy(LinearGaussian::P_tt);
    filter_estimates[py::str("v_t")] = cubepy(LinearGaussian::v_t);
    filter_estimates[py::str("F_t")] = cubepy(LinearGaussian::F_t);
    filter_estimates[py::str("K_t")] = cubepy(LinearGaussian::K_t);
  } // void filter

  void smoother() {
    LinearGaussian::smoother();
    smoother_estimates[py::str("a_tn")] = cubepy(LinearGaussian::a_tn);
    smoother_estimates[py::str("P_tn")] = cubepy(LinearGaussian::P_tn);
    smoother_estimates[py::str("a_0n")] = matpy(LinearGaussian::a_0n);
    smoother_estimates[py::str("P_0n")] = matpy(LinearGaussian::P_0n);
    smoother_estimates[py::str("P_tt1n")] = cubepy(LinearGaussian::P_tt1n);
  } // void smoother


  void update_model(pyarray &Y_arr, pyarray &T_arr, pyarray &Z_arr, pyarray &H_arr, pyarray &Q_arr, pyarray &a0_arr, pyarray &P0_arr,
                    pyarray &K_arr, pyarray &L_arr, pyarray &x_arr, pyarray &c_arr, pyarray &d_arr) {
//  it's not necessary to update all system matrices
    arma::cube Y = pycube(Y_arr);
    arma::mat T = pymat(T_arr);
    arma::cube Z = pycube(Z_arr);
    arma::mat H = pymat(H_arr);
    arma::mat Q = pymat(Q_arr);
    arma::vec a0 = pymat(a0_arr);
    arma::mat P0 = pymat(P0_arr);
    arma::mat K = pymat(K_arr);
    arma::mat L = pymat(L_arr);
    arma::cube x = pycube(x_arr);
    arma::vec c = pymat(c_arr);
    arma::vec d = pymat(d_arr);
  LinearGaussian::UpdateData(Y, T, Z, H, Q, a0, P0, K, L, x, c, d);
  }

};
