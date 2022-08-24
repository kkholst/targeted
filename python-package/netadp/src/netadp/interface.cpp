#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)
#include <pybind11/stl.h>
#include <target/utils.hpp>
#include <target/glm.hpp>
#include "loop.hpp"
#include "armapy.hpp"
#include "py_test.hpp"

pyarray myloop1(pyarray &x) {
  arma::mat res = myloop(pymat(x));
  return matpy(res);
}

pyarray expit(pyarray &x) {
  arma::mat res = target::expit(pymat(x));
  return matpy(res);
}

PYBIND11_MODULE(__netadp_c__, m) {
  m.doc() = "Python bindings for the target C++ library";

  m.def("myloop", &myloop1, "test loop");
  m.def("expit", &expit, "Sigmoid function (inverse logit)");
  //m.#define ("expit", [](arma::mat &x) { return target::expit(x); }, "Sigmoid function (inverse logit)");

  py::class_<Test>(m, "testclass")
    .def(py::init<pyarray &, pyarray &,  // x, y
       std::string>())                   // model-type: 'default', 'advanced'
    .def("calc", &Test::calc);
}
