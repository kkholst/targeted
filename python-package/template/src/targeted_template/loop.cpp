#include "loop.hpp"

std::vector<double> myloop(std::vector<double> x) {
  for (unsigned i=0; i<x.size(); i++) {
    x[i] = x[i]+1;
  }
  return x;
}

void scale2(Eigen::Ref<Eigen::MatrixXd> v) {
    v *= 2;
}

Eigen::MatrixXd add(Eigen::MatrixXd x,
                    Eigen::MatrixXd y) {
  return x+y;
}
