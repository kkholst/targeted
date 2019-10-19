#pragma once

#include "target.hpp"
#include "glm.hpp"
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)
#include <cfloat>     // precision of double (DBL_MIN)
#include <functional> // std::bind for using non-static member function as argument to free function

using complex = std::complex<double>;
using cxfunc  = std::function<arma::cx_mat(arma::cx_vec theta)>;

class RiskReg {
  
public:
  RiskReg(const arma::vec &y, const arma::vec &a,
  	  const arma::mat &x1, const arma::mat &x2, const arma::mat &x3,
  	  const arma::vec &weights, std::string Model) {
    Y = y; A = a; X1 = x1; X2 = x2; X3 = x3; W = weights;
    this->Model = Model;
    arma::vec par = arma::zeros(X1.n_cols + X2.n_cols + X3.n_cols);
    if (Model.compare("rr")) {       
      model.reset(new target::RR<double>(Y, A, X1, X2, X3, par, W));
    } else {
      model.reset(new target::RR<double>(Y, A, X1, X2, X3, par, W));
    }
  }
  ~RiskReg() {}

  void update(arma::vec &par) {
    this->theta = par;
    model->updatePar(par);
    model->calculate(true, true, true);
  }  
  double logl(arma::vec &par) {
    update(par);
    return model->loglik(false)[0];
  }
  arma::mat dlogl(arma::vec &par) {
    update(par);
    return model->score(false);
  }
  arma::mat pr(arma::vec &par) {
    update(par);
    return model->pa();
  }

private:
  std::unique_ptr< target::TargetBinary<double> >   model;
  std::unique_ptr< target::TargetBinary<complex> >  model_c;
  arma::vec Y;
  arma::vec A;
  arma::mat X1;
  arma::mat X2;
  arma::mat X3;
  arma::vec W;
  arma::vec theta;
  std::string Model;		  
};
