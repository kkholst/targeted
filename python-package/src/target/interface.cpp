#include "armapy.hpp"
#include "target.hpp"
#include "glm.hpp"
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)


pyarray expit(pyarray &x) {
  arma::mat res = target::expit(pymat(x));
  return matpy(res);
}

class RiskReg {
 
  
public:
  using cx_dbl = target::cx_dbl;
  using cx_func = target::cx_func;

  RiskReg(pyarray &y, pyarray &a,
	  pyarray &x1, pyarray &x2, pyarray &x3,
	  pyarray &weights, std::string Model) {
    Y = pymat(y).as_col();
    A = pymat(a).as_col();
    X1 = pymat(x1);
    X2 = pymat(x2);
    X3 = pymat(x3);
    theta = arma::zeros(X1.n_cols + X2.n_cols + X3.n_cols);
    W = pymat(weights).as_col();
    this->Model = Model;
    
    if (Model.compare("rr") == 0) {           
      model.reset(new target::RR<double>(Y, A, X1, X2, X3, theta, W));
    } else {
      model.reset(new target::RD<double>(Y, A, X1, X2, X3, theta, W));
    }
  }
  ~RiskReg() {}

  void update(pyarray &par) {
    arma::vec theta = pymat(par).as_col();
    for (unsigned i=0; i<theta.n_elem; i++)
      this->theta(i) = theta(i);
    model->updatePar(theta);
    model->calculate(true, true, true);
  }
  pyarray pr() {
    arma::mat res = model->pa();
    return matpy(res);
  }  
  double logl() {
    double res = model->loglik(false)[0];
    return res;
  }  
  pyarray dlogl(bool indiv=false) {
    arma::mat res = model->score(indiv);
    return matpy(res);
  }
  pyarray esteq(pyarray &par, pyarray &pred) {
    arma::vec alpha = pymat(par).as_col();
    arma::vec pr = pymat(pred).as_col();
    arma::vec res = model->est(alpha, pr);
    return matpy(res);
  }
  
  arma::cx_mat score(arma::cx_vec theta) {
    model_c->updatePar(theta);
    model_c->calculate(true, true, false);
    return model_c->score(false);
  }
  pyarray hessian() {
    arma::cx_vec Yc = arma::conv_to<arma::cx_vec>::from(Y);
    arma::cx_vec Ac = arma::conv_to<arma::cx_vec>::from(A);
    arma::cx_mat X1c = arma::conv_to<arma::cx_mat>::from(X1);
    arma::cx_mat X2c = arma::conv_to<arma::cx_mat>::from(X2);
    arma::cx_mat X3c = arma::conv_to<arma::cx_mat>::from(X3);
    arma::cx_vec thetac = arma::conv_to<arma::cx_vec>::from(theta);
    arma::cx_vec Wc = arma::conv_to<arma::cx_vec>::from(W);
    if (Model.compare("rr") == 0) {           
      model_c.reset(new target::RR<cx_dbl>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));
    } else {
      model_c.reset(new target::RR<cx_dbl>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));      
    }
    using namespace std::placeholders;
    arma::mat res = target::deriv(std::bind(&RiskReg::score, this, _1), theta);
    return matpy(res);
  }


private:
  std::unique_ptr< target::TargetBinary<double> >   model;
  std::unique_ptr< target::TargetBinary<cx_dbl> >  model_c;
  arma::vec Y;
  arma::vec A;
  arma::mat X1;
  arma::mat X2;
  arma::mat X3;
  arma::vec W;
  arma::vec theta;
  std::string Model;		  
};


PYBIND11_MODULE(target_c, m) {
  m.doc() = "Python bindings for the Target C++ library";
  
  m.def("expit", &expit, "Sigmoid function (inverse logit)"); 
  
  py::class_<RiskReg>(m, "riskregmodel")
    .def(py::init<pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, std::string>())
    .def("update", &RiskReg::update)
    .def("pr", &RiskReg::pr)
    .def("score", &RiskReg::dlogl, py::arg("indiv") = false)
    .def("esteq", &RiskReg::esteq)
    .def("hessian", &RiskReg::hessian)
    .def("loglik", &RiskReg::logl);
}
