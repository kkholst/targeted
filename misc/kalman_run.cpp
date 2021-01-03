
#include <armadillo>
#include <iostream>
#include <ostream>
#include <target/kalman.hpp>
#include <target/utils.hpp>

using namespace arma;
using std::string;
using std::cout;
using std::endl;


int main(int argc, char **argv) {
  cout << target::RED << "Kalman filter test\n\n";
  mat H(4, 4);
  H.fill(10);
  mat Q = arma::ones(4, 4);
  vec y(2);
  target::KalmanFilter KF(H, Q, y);
  mat res = KF.filter();
  cout << res << endl;
  cout << target::COL_RESET;
  return 0;
}

