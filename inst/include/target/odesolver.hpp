/*!
  @file odesolver.hpp
  @author Klaus K. Holst
  @copyright 2019, Klaus Kähler Holst

  @brief Classes for Ordinary Differential Equation Solvers

  Explicit Runge-Kutta methods

*/

#pragma once
#include "utils.hpp"

namespace target {


  /*!
   * Butcher Tableau Class
   * 0  |
   * c2 | a[21]
   * c3 | a[31] a[32]
   * .  | .     .
   * .  | .     .
   * .  | .     .
   * cs | a[s1] a[s2] ... a[s,s-1]
   * ---------------------------------
   *      b1    b2    ... b[s-1]    bs
   *      B1    B2    ... B[s-1]    B[s]
   *
   *  y[n+1] = y[n] + h sum b[s]k[s]
   *  Y[n+1] = Y[n] + h sum B[s]k[s]  ## Lower-order approx.
   *
   *  k[1] = f(t[n], y[n])
   *  k[2] = f(t[n] + h*c2, y[n] + h{k[1]*a[21]})
   *  k[3] = f(t[n] + h*c3, y[n] + h{k[1]*a[31] + k[2]*a[32]})
   *  .
   *  .
   *  .
   *  k[s] = f(t[n] + h*c[s], y[n] + h{k[1]*a[s1] + k[s-1]*a[s,s-1]})
   */
  class ButcherTableau {
    public:
      arma::mat A;
      arma::mat B;
      arma::colvec c;


  };

//


  // Type definition
  using odefunc = std::function<arma::mat(arma::mat input,
    arma::mat x,
    arma::mat theta)>;

  arma::mat interpolate(const arma::mat &input,  // first column is time
                        double tau,              // Time-step
                        bool locf = false);      // Last-observation-carried forward
                                                 // otherwise linear interpolation
  /*!
    Abstract class for ODE Solver
  */
  class Solver {
  protected:
    odefunc F;

  public:
    explicit Solver(odefunc F) {
      this->F = F;
    }
    virtual ~Solver() {}

    virtual arma::mat solve(const arma::mat &input,
                            arma::mat init,
                            arma::mat theta) = 0;
    arma::mat solveint(const arma::mat &input,
                       arma::mat init,
                       arma::mat theta,
                       double tau = 1.0e-1,
                       bool reduce = true);
  };

  /*!
    Runge-Kutta solver
   */
  class RK4 : public Solver {  // Basic 4th order Runge-Kutta solver
    public:
      using Solver::Solver;

    arma::mat solve(const arma::mat &input, arma::mat init, arma::mat theta);
    };

  }  // namespace target
