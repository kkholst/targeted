#include "armapy.hpp"
#include "target.hpp"
#include "glm.hpp"
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)
#include <cfloat>     // precision of double (DBL_MIN)
#include <functional> // std::bind for using non-static member function as argument to free function

using complex = std::complex<double>;
using cxfunc  = std::function<arma::cx_mat(arma::cx_vec theta)>;

pyarray expit(pyarray &x) {
  arma::mat res = glm::expit(pymat(x));
  return matpy(res);
}

arma::mat deriv(cxfunc f, arma::vec theta) {
  arma::cx_vec thetac = arma::conv_to<arma::cx_vec>::from(theta);
  arma::cx_mat val0 = f(thetac);
  unsigned n = val0.n_elem;
  unsigned p = theta.n_elem;
  arma::mat res(n,p);
  double h = DBL_MIN;
  complex h0 = complex(0, h);
  for (unsigned i=0; i<p; i++) {
    arma::cx_vec theta0 = thetac;
    theta0[i] += h0;
    arma::mat val = imag(f(theta0))/h;
    for (unsigned j=0; j<n; j++)
      res(j,i) = val[j];      
  }
  return(res);
}

class RiskReg {
public:
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
      model_c.reset(new target::RR<complex>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));
    } else {
      model_c.reset(new target::RR<complex>(Yc, Ac, X1c, X2c, X3c, thetac, Wc));      
    }
    using namespace std::placeholders;
    arma::mat res = deriv(std::bind(&RiskReg::score, this, _1), theta);
    return matpy(res);
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
