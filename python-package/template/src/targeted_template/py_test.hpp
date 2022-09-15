#include <pybind11/stl.h>
#include <vector>
#include <target/utils.hpp>



class Test {
// private:
//     arma::mat x;
//     arma::mat y;

public:
    std::vector<double> x;
    std::vector<double> y;

    Test(std::vector<double> &x, std::vector<double> &y, std::string type) {
      this->x = x;
      this->y = y;
    }

    void update_x(std::vector<double> &x) {
      this->x = x;
    }

    std::vector<double> calc() {
      std::vector<double> res = this->x;
      for (unsigned i=0; i<x.size(); i++)
        res[i] += y[i];
      return res;
    }
};
