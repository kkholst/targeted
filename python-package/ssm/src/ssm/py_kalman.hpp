#include <pybind11/stl.h>
#include <target/kalman.hpp>
#include "armapy.hpp"

class KalmanFilterPy : public target::KalmanFilter {

public:
  KalmanFilterPy(pyarray &h, pyarray &q, pyarray &y) {
    arma::mat H = pymat(h);
    arma::mat Q = pymat(q);
    arma::mat Y = pymat(y);
    KalmanFilter::UpdateData(H, Q, Y);
  }

  pyarray logl() {
    arma::mat res = KalmanFilter::filter();
    return matpy(res);
  }

};
