/*!
  @file odesolver.h
  @author Klaus K. Holst
  @copyright 2019, Klaus Kähler Holst

  @brief Classes for Ordinary Differential Equation Solvers

*/

#include <target/odesolver.hpp>
#include <target/utils.hpp>

using namespace arma;

namespace target {

  arma::mat RungeKutta::solve(const arma::mat &input,
                       arma::mat init,
                       arma::mat theta) {
    unsigned n = input.n_rows;
    unsigned p = init.n_elem;
    mat res(n, p);
    rowvec y = arma::conv_to<arma::rowvec>::from(init);
    res.row(0) = y;
    unsigned s = (this->BT).A.n_rows;
    for (unsigned i=0; i < n-1; i++)  {
      rowvec dinput = input.row(i + 1) - input.row(i);
      double tau = dinput(0);
      mat k(s, p);
      k.row(0) = (this->F)(input.row(i), y, theta);
      for (unsigned j = 1; j < s; j++) {
        // a[j1]*k[1] + ... + a[j(s-1)]*k[s-1]
        rowvec K(p, fill::zeros);
        for (unsigned l = 0; l <= (j - 1); l++) {
          K += (this->BT).A(j,l)*k.row(l);
        }
        k.row(j) = this->F(input.row(i) + (this->BT).c[j] * tau,
                           y + K*tau,
                           theta);
      }
      for (unsigned j = 0; j < s; j++) {
        y += tau*(this->BT).B(0,j) * k[j];
      }
      res.row(i+1) = y;
    }
    return( res );
  }


  arma::mat RK4::solve(const arma::mat &input,
                       arma::mat init,
                       arma::mat theta) {
    unsigned n = input.n_rows;
    unsigned p = init.n_elem;
    mat res(n, p);
    rowvec y = arma::conv_to<arma::rowvec>::from(init);
    res.row(0) = y;
    for (unsigned i=0; i < n-1; i++)  {
      rowvec dinput = input.row(i+1)-input.row(i);
      double tau = dinput(0);
      rowvec f1 = tau*F(input.row(i),            y,        theta);
      rowvec f2 = tau*F(input.row(i) + dinput/2, y + f1/2, theta);
      rowvec f3 = tau*F(input.row(i) + dinput/2, y + f2/2, theta);
      rowvec f4 = tau*F(input.row(i) + dinput,   y + f3,   theta);
      y += (f1+2*f2+2*f3+f4)/6;
      res.row(i+1) = y;
    }
    return( res );
  }

  arma::mat Solver::solveint(const arma::mat &input,
                             arma::mat init,
                             arma::mat theta,
                             double tau, bool reduce) {
    mat newinput = interpolate(input, tau, true);
    mat value = solve(newinput, init, theta);
    if (reduce) {
      uvec idx = target::fastapprox(newinput.col(0), input.col(0), false, 0);
      value = value.rows(idx);
    }
    return( value );
  }

}  // namespace target
