
#pragma once

#include "target.hpp"
#include "glm.hpp"
#include "utils.hpp"
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)


enum Data {Y, A, X1, X2, X3, W};

class RiskReg {

public:
  using cx_dbl = target::cx_dbl;
  using cx_func = target::cx_func;

  RiskReg() {}
  RiskReg(const arma::vec &y, const arma::vec &a,
  	  const arma::mat &x1, const arma::mat &x2, const arma::mat &x3,
  	  const arma::vec &weights, std::string model) {
    this->type = model;
    setData(y, a, x1, x2, x3, weights);
  }
  ~RiskReg() {}

  void setData(const arma::vec &y, const arma::vec &a,
	       const arma::mat &x1, const arma::mat &x2, const arma::mat &x3,
	       const arma::vec &weights) {
    theta = arma::zeros(x1.n_cols + x2.n_cols + x3.n_cols);
    if (this->type.compare("rr") == 0) {
      this->model.reset(new target::RR<double>(y, a, x1, x2, x3, theta, weights));
    } else {
      this->model.reset(new target::RD<double>(y, a, x1, x2, x3, theta, weights));
    }
  }

  void weights(const arma::vec &weights) {
    model->weights(weights);
  }

  void update(arma::vec &par) {
    for (unsigned i=0; i<par.n_elem; i++)
      this->theta(i) = par(i);
    model->update_par(par);
    model->calculate(true, true, true);
  }
  double logl() {
    return model->loglik(false)[0];
  }
  arma::mat dlogl(bool indiv=false) {
    return model->score(indiv);
  }
  arma::mat pr() {
    return model->pa();
  }

  arma::cx_mat score(arma::cx_vec theta) {
    model_c->update_par(theta);
    model_c->calculate(true, true, false);
    return model_c->score(false);
  }

  arma::mat esteq(arma::vec &alpha, arma::vec &pr) {
    arma::mat res = model->est(alpha, pr);
    return res;
  }

  arma::mat hessian() {
    arma::cx_vec Yc = arma::conv_to<arma::cx_vec>::from(model->Y());
    arma::cx_vec Ac = arma::conv_to<arma::cx_vec>::from(model->A());
    arma::cx_mat X1c = arma::conv_to<arma::cx_mat>::from(model->X1());
    arma::cx_mat X2c = arma::conv_to<arma::cx_mat>::from(model->X2());
    arma::cx_mat X3c = arma::conv_to<arma::cx_mat>::from(model->X3());
    arma::cx_vec thetac = arma::conv_to<arma::cx_vec>::from(theta);
    arma::cx_vec Wc = arma::conv_to<arma::cx_vec>::from(model->weights());
    if (this->type.compare("rr") == 0) {
      model_c.reset(new target::RR<cx_dbl>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));
    } else {
      model_c.reset(new target::RD<cx_dbl>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));
    }
    using namespace std::placeholders;
    arma::mat res = target::deriv(std::bind(&RiskReg::score, this, _1), theta);
    return res;
  }

  arma::mat operator()(Data index) const {
    switch(index) {
    case Y:
      return model->Y();
    case A:
      return model->A();
    case X1:
      return model->X1();
    case X2:
      return model->X2();
    case X3:
      return model->X3();
    case W:
      break;
    }
    return model->weights();
  }

protected:
  std::unique_ptr< target::TargetBinary<double> >   model;
  std::unique_ptr< target::TargetBinary<cx_dbl> >  model_c;
  arma::vec theta;
  std::string type;
};
