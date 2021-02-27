/*!
  @file odesolver.hpp
  @author Klaus K. Holst
  @copyright 2019, Klaus Kähler Holst

  @brief Classes for Ordinary Differential Equation Solvers

  4th order Runge-Kutta
  
*/

#pragma once
#include "utils.hpp"

namespace target {

  // Type definition
  using odefunc = std::function<arma::mat(arma::mat input,
    arma::mat x,
    arma::mat theta)>;

  arma::mat interpolate(const arma::mat &input,  // first column is time
                        double tau,  // Time-step
                        bool locf = false);  // Last-observation-carried forward
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
