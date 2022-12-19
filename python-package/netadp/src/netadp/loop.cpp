#include "loop.hpp"

arma::mat myloop(arma::mat x) {
  arma::mat res = x;
  for (unsigned i=0; i<x.n_elem; i++) {
    res[i] = x[i]+1;
  }
  return res;
}
