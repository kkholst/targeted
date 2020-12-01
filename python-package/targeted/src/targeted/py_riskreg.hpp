#include <pybind11/stl.h>
#include <target/riskreg.hpp>
#include <target/utils.hpp>
#include <target/glm.hpp>
#include "armapy.hpp"


class RiskRegPy : public RiskReg {

public:
  RiskRegPy(pyarray &y, pyarray &a,
	    pyarray &x1, pyarray &x2, pyarray &x3,
	    pyarray &weights, std::string Model) {
    arma::vec Y = pymat(y).as_col();
    arma::vec A = pymat(a).as_col();
    arma::mat X1 = pymat(x1);
    arma::mat X2 = pymat(x2);
    arma::mat X3 = pymat(x3);
    arma::vec theta = arma::zeros(X1.n_cols + X2.n_cols + X3.n_cols);
    arma::vec W = pymat(weights).as_col();
    this->type = Model;
    RiskReg::setData(Y, A, X1, X2, X3, W);
  }

  void weights(pyarray &weights) {
    arma::vec w = pymat(weights).as_col();
    RiskReg::weights(w);
  }
  void update(pyarray &par) {
    arma::vec theta = pymat(par).as_col();
    RiskReg::update(theta);
  }
  pyarray pr() {
    arma::mat res = RiskReg::pr();
    return matpy(res);
  }
  double logl() {
    return RiskReg::logl();
  }
  pyarray dlogl(bool indiv=false) {
    arma::mat res = RiskReg::dlogl(indiv);
    return matpy(res);
  }

  pyarray esteq(pyarray &par, pyarray &pred) {
    arma::vec alpha = pymat(par).as_col();
    arma::vec pr = pymat(pred).as_col();
    arma::mat res  = RiskReg::esteq(alpha, pr);
    return matpy(res);
  }
  pyarray hessian() {
    arma::mat res = RiskReg::hessian();
    return matpy(res);
  }

  pyarray data(Data idx) {
    arma::mat res = RiskReg::operator()(idx);
    return matpy(res);
  }

};
