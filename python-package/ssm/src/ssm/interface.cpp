#include <pybind11/stl.h>
#include <target/utils.hpp>
#include "armapy.hpp"
#include "py_kalman.hpp"

// file that links python and c++ classes/methods

PYBIND11_MODULE(__ssm_c__, m) {
  m.doc() = "Python bindings for the target C++ library";

  py::class_<LinearGaussianPy>(m, "CPP_Kalman")
//    .def(py::init<pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &>())  // Y,T,Z,H,Q,a0,P0
    .def(py::init<pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray &, pyarray & >())  // Y,T,Z,H,Q,a0,P0, K, L, x, c, d
    .def("filter", &LinearGaussianPy::filter)
    .def("smoother", &LinearGaussianPy::smoother)
    .def("update_model", &LinearGaussianPy::update_model)
    .def_readonly("llh",&LinearGaussianPy::llh)
    .def_readonly("filter_estimates",&LinearGaussianPy::filter_estimates) // dictionary with output of filter algorithm (i.e. a_t,P_t,a_tt,P_tt, K_t, v_t, F_t)
    .def_readonly("smoother_estimates",&LinearGaussianPy::smoother_estimates) // dictionary with output of smoother algorithm (i.e. a_tn,P_tn,a_0n,P_0n, P_tt1n)
    ;
}
