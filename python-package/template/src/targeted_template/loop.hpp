#pragma once

#include <vector>
#include <pybind11/eigen.h>

std::vector<double> myloop(std::vector<double>);

void scale2(Eigen::Ref<Eigen::MatrixXd>);

Eigen::MatrixXd add(Eigen::MatrixXd,
                    Eigen::MatrixXd);
