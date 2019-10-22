#include "armapy.hpp"
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

};

// [[Rcpp::export]]
// Rcpp::List ace_est(const arma::vec &y,
// 		   const arma::mat &a,
// 		   const arma::mat &x1,
// 		   const arma::mat &x2,
// 		   const arma::vec &theta,
// 		   const arma::vec &weights,
// 		   bool binary = true) {
//   arma::vec par(theta.n_elem+1);
//   par[0] = 0;
//   for (unsigned i=0; i < theta.n_elem; i++) par[i+1] = theta[i];
//   target::ACE model(y, a, x1, x2, par, weights, binary);
//   double alpha = real(model.est(false)[0])/y.n_elem;
//   par[0] = alpha;
//   model.update_par(par);
//   model.calculate();
//   arma::vec U = real(model.est(true));
//   arma::mat dU = model.deriv();
//   return ( Rcpp::List::create(Rcpp::Named("alpha") = alpha,
// 			      Rcpp::Named("u") = U,
// 			      Rcpp::Named("du") = dU) );
// }


PYBIND11_MODULE(target_c, m) {
  m.doc() = "Python bindings for the Target C++ library";

  m.def("expit", &expit, "Sigmoid function (inverse logit)");

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
    .def("loglik", &RiskRegPy::logl);
}
