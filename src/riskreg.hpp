#pragma once

#include <string>
#include <memory>
#include "target.hpp"
#include "glm.hpp"

class RiskReg {
  
public:
  RiskReg(arma::vec y, arma::vec a,
  	  arma::mat x1, arma::mat x2,
  	  arma::vec weights, std::string Model) {

    arma::vec par = arma::zeros(x1.n_cols + x2.n_cols); //     
    if (Model.compare("rr")) {       
      model.reset(new target::RR<double>(y, a, x1, x2, x2, par, weights));
    } else {
      model.reset(new target::RR<double>(y, a, x1, x2, x2, par, weights));
    }
  }
  ~RiskReg() {}

  void update(arma::Col<double> &par) {
    model->updatePar(par);
    model->calculate(true, true, false);
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
  std::unique_ptr< target::TargetBinary<double> >  model;

};
