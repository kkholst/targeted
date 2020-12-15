#include <pybind11/stl.h>
#include <target/utils.hpp>
#include "armapy.hpp"
#include "py_kalman.hpp"


PYBIND11_MODULE(__ssm_c__, m) {
  m.doc() = "Python bindings for the target C++ library";

  py::class_<KalmanFilterPy>(m, "kalmanfilter")
    .def(py::init<pyarray &, pyarray &, pyarray &>())  // H, Q, y
    .def("loglik", &KalmanFilterPy::logl);

}
