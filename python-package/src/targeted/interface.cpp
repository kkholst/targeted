#include <pybind11/stl.h>
#include "armapy.hpp"
#include "cumres.hpp"
#include "utils.hpp"
#include "glm.hpp"
#include "riskreg.hpp"
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)

pyarray expit(pyarray &x) {
  arma::mat res = target::expit(pymat(x));
  return matpy(res);
}

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

using matrices = std::vector<pyarray>;
matrices ace_est(pyarray &y,
		 pyarray &a,
		 pyarray &x1,
		 pyarray &x2,
		 pyarray &par,
		 pyarray &weights,
		 bool binary = true) {
  arma::vec Y = pymat(y).as_col();
  arma::vec A = pymat(a).as_col();
  arma::mat X1 = pymat(x1);
  arma::mat X2 = pymat(x2);
  arma::vec W = pymat(weights).as_col();
  arma::vec theta = pymat(par).as_col();  
  arma::vec par0(theta.n_elem+1);
  par0[0] = 0;
  for (unsigned i=0; i < theta.n_elem; i++) par0[i+1] = theta[i];
  target::ACE model(Y, A, X1, X2, par0, W, binary);
  double alpha = real(model.est(false)[0])/Y.n_elem;
  par0[0] = alpha;
  model.update_par(par0);
  model.calculate();
  arma::mat U = real(model.est(true));
  arma::mat dU = model.deriv();
  matrices res;
  arma::mat alphamat(1,1); alphamat[0] = alpha;
  res.push_back(matpy(alphamat));
  res.push_back(matpy(U));
  res.push_back(matpy(dU));
  return res;
}


PYBIND11_MODULE(__targeted_c__, m) {
  m.doc() = "Python bindings for the target C++ library";

  m.def("expit", &expit, "Sigmoid function (inverse logit)");
  //m.def("expit", [](arma::mat &x) { return target::expit(x); }, "Sigmoid function (inverse logit)");

  m.def("ace_est", &ace_est, "Average Causal Effect estimation");
 
  py::enum_<Data>(m, "datatype")
    .value("y", Y)
    .value("a", A)
    .value("x1", X1)
    .value("x2", X2)
    .value("x3", X3)
    .value("w", W)    
    .export_values();

  py::class_<RiskRegPy>(m, "riskregmodel")
    .def(py::init<pyarray &, pyarray &,   // y, a
       pyarray &, pyarray &, pyarray &, // x1, x2, x3
       pyarray &,                       // weights
       std::string>())                  // model-type: 'rr', 'rd'
    .def("update", &RiskRegPy::update)
    .def("pr", &RiskRegPy::pr)
    .def("score", &RiskRegPy::dlogl, py::arg("indiv") = false)
    .def("esteq", &RiskRegPy::esteq)
    .def("hessian", &RiskRegPy::hessian)
    .def("loglik", &RiskRegPy::logl)
    .def("data", &RiskRegPy::data)
    .def("weights", &RiskRegPy::weights)
    
    ;
       //  .def("Y",  [](RiskRegPy &a, Data idx) { return a(idx); };)
}
