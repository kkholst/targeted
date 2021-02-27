
#include <armadillo>
#include <iostream>
#include <ostream>
#include <target/odesolver.hpp>
#include <target/utils.hpp>

using arma::vec;
using arma::mat;
using std::cout;
using std::endl;

vec dy(const vec &input,  // time (first element) and input variables
       const vec &x,      // state variables
       const vec &par) {
  return par(0) + par(1)*x;
}


int main(int argc, char **argv) {
    cout << target::BLUE << "RK4 test\n\n";

    target::RK4 MyODE(dy);
    vec t = arma::linspace(0, 2, 20);
    vec y0 = arma::zeros(1);
    vec par = { 1.0, 1.0 };
    vec y = MyODE.solve(t, y0, par);
    cout << y << endl;
    std::cout << target::COL_RESET;

    mat ty = arma::join_horiz(t, y);
    ty.save("y.csv", arma::csv_ascii);

    return 0;
}

