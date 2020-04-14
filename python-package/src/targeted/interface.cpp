#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)
#include <pybind11/stl.h>
#include <target/utils.hpp>
#include <target/glm.hpp>
#include "armapy.hpp"
#include "py_riskreg.hpp"


pyarray expit(pyarray &x) {
  arma::mat res = target::expit(pymat(x));
  return matpy(res);
}

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
    .def("weights", &RiskRegPy::weights);
  //  .def("Y",  [](RiskRegPy &a, Data idx) { return a(idx); };)
}
