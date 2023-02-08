#pragma once

#include <armadillo>
#include <pybind11/pybind11.h>
#include <pybind11/numpy.h>
#include <vector>

namespace py = pybind11;

using pyarray = py::array_t<double, py::array::c_style | py::array::forcecast>;

arma::mat pymat(pyarray &array) {
  unsigned nc = 1, nr = array.shape()[0];;
  if (array.ndim()==2) {
    nc = array.shape()[1];
  }
  arma::mat x(&array.data()[0], nc, nr); //, true);
  return x.t();
}


arma::cube pycube(pyarray &array) {
    unsigned ns = array.shape()[0], nr = array.shape()[1], nc=array.shape()[2];
    arma::cube x(&array.data()[0], nc,nr, ns);

    // arma::cube x(&array.data()[0], nr,nc, ns); does not result in a cube that corresponds to the
    // 3d input numpy array
    arma::cube d(nr,nc,ns);
    for (int i = 0; i < ns; i++) {d.slice(i) = x.slice(i).t();}
    //    TODO: can I do this more efficient?
    return d;
}


pyarray matpy(arma::mat &x) {
  ssize_t              ndim    = 2;
  std::vector<ssize_t> shape   = { (ssize_t)x.n_rows , (ssize_t)x.n_cols };
  std::vector<ssize_t> strides = { (ssize_t)(sizeof(double)) , (ssize_t)(sizeof(double)*x.n_rows) };  
  // return 2-D NumPy array
  return py::array(py::buffer_info(
    &x[0],                                   /* data as contiguous array  */
    sizeof(double),                          /* size of one scalar        */
    py::format_descriptor<double>::format(), /* data type                 */
    ndim,                                    /* number of dimensions      */
    shape,                                   /* shape of the matrix       */
    strides                                  /* strides for each axis     */
  ));
}



pyarray cubepy(arma::cube &x) {
  ssize_t              ndim    = 3;
  std::vector<ssize_t> shape   = { (ssize_t)x.n_slices, (ssize_t)x.n_rows, (ssize_t)x.n_cols};
  std::vector<ssize_t> strides = { (ssize_t)(sizeof(double)*x.n_rows*x.n_cols), (ssize_t)(sizeof(double)) , (ssize_t)(sizeof(double)*x.n_cols)};
  // return 3-D NumPy array
  return py::array(py::buffer_info(
    &x[0],                                   /* data as contiguous array  */
    sizeof(double),                          /* size of one scalar        */
    py::format_descriptor<double>::format(), /* data type                 */
    ndim,                                    /* number of dimensions      */
    shape,                                   /* shape of the matrix       */
    strides                                  /* strides for each axis     */
  ));
}
