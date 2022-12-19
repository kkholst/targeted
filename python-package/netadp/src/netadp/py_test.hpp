#pragma once

#include <pybind11/stl.h>
#include <target/utils.hpp>
#include "armapy.hpp"


class Test {

private:
    arma::mat x;
    arma::mat y;

public:
    Test(pyarray &x, pyarray &y, std::string type) {
      this->x = pymat(x);
      this->y = pymat(y);
    }

    void update_x(pyarray &x) {
      this->x = pymat(x);
    }

    pyarray calc() {
      arma::mat res = x+y;
      return matpy(res);
    }
};
