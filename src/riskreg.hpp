#pragma once

#include "target.hpp"
#include "glm.hpp"
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)

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
    Y = y; A = a; X1 = x1; X2 = x2; X3 = x3; W = weights;
    theta = arma::zeros(X1.n_cols + X2.n_cols + X3.n_cols);
    if (this->type.compare("rr") == 0) {       
      this->model.reset(new target::RR<double>(Y, A, X1, X2, X3, theta, W));
    } else {
      this->model.reset(new target::RD<double>(Y, A, X1, X2, X3, theta, W));
    }
  }

  void update(arma::vec &par) {
    for (unsigned i=0; i<par.n_elem; i++)
      this->theta(i) = par(i);
    model->updatePar(par);
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
    model_c->updatePar(theta);
    model_c->calculate(true, true, false);
    return model_c->score(false);
  }

  arma::mat esteq(arma::vec &alpha, arma::vec &pr) {
    arma::vec res = model->est(alpha, pr);
    return res;
  }  

  arma::mat hessian() {
    arma::cx_vec Yc = arma::conv_to<arma::cx_vec>::from(Y);
    arma::cx_vec Ac = arma::conv_to<arma::cx_vec>::from(A);
    arma::cx_mat X1c = arma::conv_to<arma::cx_mat>::from(X1);
    arma::cx_mat X2c = arma::conv_to<arma::cx_mat>::from(X2);
    arma::cx_mat X3c = arma::conv_to<arma::cx_mat>::from(X3);
    arma::cx_vec thetac = arma::conv_to<arma::cx_vec>::from(theta);
    arma::cx_vec Wc = arma::conv_to<arma::cx_vec>::from(W);
    if (this->type.compare("rr") == 0) {           
      model_c.reset(new target::RR<cx_dbl>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));
    } else {
      model_c.reset(new target::RD<cx_dbl>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));      
    }
    using namespace std::placeholders;
    arma::mat res = target::deriv(std::bind(&RiskReg::score, this, _1), theta);
    return res;
  }
  

protected:
  std::unique_ptr< target::TargetBinary<double> >   model;
  std::unique_ptr< target::TargetBinary<cx_dbl> >  model_c;
  arma::vec Y;
  arma::vec A;
  arma::mat X1;
  arma::mat X2;
  arma::mat X3;
  arma::vec W;
  arma::vec theta;
  std::string type;
};
