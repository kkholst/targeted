/*!
  @file target.h
  @author Klaus K. Holst
  @copyright 2018, Klaus Kähler Holst

  @brief Classes for targeted inference models

*/

#ifndef SRC_TARGET_H_
#define SRC_TARGET_H_

#ifndef ARMA_R
#define MATHLIB_STANDALONE
#include <armadillo>
//#include "Rmath.h"
#endif
#if defined(ARMA_R)
#define ARMA_DONT_USE_OPENMP
#include <RcppArmadillo.h>
#endif
#include <complex>
typedef std::complex<double> Complex;
#include "glm.h"

namespace target {

  /*!
    Abstract class for Targeted Learning problems
  */
  template <class T>
  class Target {
  protected:
    arma::Col<T> nuisance;
    arma::Col<T> target;
    arma::Col<T> propensity;

    const arma::Col<T> *_response;
    const arma::Mat<T> *_exposure;
    const arma::Mat<T> *_x1;
    const arma::Mat<T> *_x2;
    const arma::Mat<T> *_x3;
    const arma::Col<T> *_weights = nullptr;

    bool useWeights = false;

    arma::Col<T> alpha;  // Target parameter
    arma::Col<T> beta;  // Nuisance parameter
    arma::Col<T> gamma;  // Propensity parameter

  public:
    //! Constructor
    // Target() {};

    Target(const arma::Col<T> &y, const arma::Mat<T> &a,
	   const arma::Mat<T> &x1, const arma::Mat<T> &x2, const arma::Mat<T> &x3,
	   const arma::Col<T> &parameter,
	   const arma::Col<T> &weights);

    Target(const arma::Col<T> &y, const arma::Mat<T> &a,
	   const arma::Mat<T> &x1, const arma::Mat<T> &x2,
	   const arma::Col<T> &parameter,
	   const arma::Col<T> &weights);
    
    Target(const arma::Col<T> &y, const arma::Mat<T> &a,
	   const arma::Mat<T> &x1, const arma::Mat<T> &x2, const arma::Mat<T> &x3,
	   const arma::Col<T> &parameter);

    Target(const arma::Col<T> &y, const arma::Mat<T> &a,
	   const arma::Mat<T> &x1, const arma::Mat<T> &x2,
	   const arma::Col<T> &parameter);
    
    virtual ~Target() {} // Abstract class

    void weights(const arma::Col<T> &weights) { _weights = &weights; }
    virtual void calculate(bool target = true,
			   bool nuisance = true,
			   bool propensity = false);
    void updatePar(const arma::Col<T> &parameter);
    arma::Col<T> weights() { return *(_weights); }
    arma::Col<T> A() { return *(_exposure); }
    arma::Col<T> Y() { return *(_response); }
    arma::Mat<T> X1() { return *_x1; }
    arma::Mat<T> X2() { return *_x2; }
    arma::Mat<T> X3() { return *_x3; }
  };


  ////////////////////////////////////////////////////////////////////////////////


  template <class T>
  class TargetBinary : public Target<T> {
  protected:
    arma::Mat<T> pr;
    virtual arma::Col<T> H() = 0;
    virtual arma::Mat<T> dp() = 0;
  public:
    using Target<T>::Target;
    virtual arma::Mat<T> pa();
    virtual arma::Mat<T> p(bool exposure = 0) { return pr.col(exposure); }
    virtual arma::Col<T> loglik(bool indiv = false);
    virtual arma::Mat<T> score(bool indiv = false);
    virtual arma::Mat<T> est(arma::Col<T> alpha,
			     const arma::Col<T> &propensity);
    virtual arma::Mat<T> est(arma::Col<T> alpha);
    void calculate(bool target = true,
			   bool nuisance = true,
			   bool propensity = false) override;
  };

  ////////////////////////////////////////////////////////////////////////////////

  template <class T>
  class RD : public TargetBinary<T> {
  private:
    arma::Col<T> H() override { return(RD<T>::Y() - RD<T>::A()%rd()); }
    arma::Mat<T> dp() override;
  public:
    RD(const arma::Col<T> &y, const arma::Mat<T> &a,
       const arma::Mat<T> &x1, const arma::Mat<T> &x2, const arma::Mat<T> &x3,
       const arma::Col<T> &parameter,
       const arma::Col<T> &weights);

    arma::Col<T> rd() { return RD<T>::target; }
    arma::Col<T> op() { return RD<T>::nuisance; }
    void calculate(bool target = true,
		   bool nuisance = true,
		   bool propensity = false) override;
  };

  template <class T>
  class RR : public TargetBinary<T> {
  private:
    arma::Col<T> H() override {
      return(RR<T>::Y()%exp( - RR<T>::A().col(0) % log( rr() ) ) );
    }
    arma::Mat<T> dp() override;

  public:
    RR(const arma::Col<T> &y, const arma::Mat<T> &x,
       const arma::Mat<T> &z1, const arma::Mat<T> &z2, const arma::Mat<T> &z3,
       const arma::Col<T> &parameter,
       const arma::Col<T> &weights);

    arma::Col<T> rr() { return RR<T>::target; }
    arma::Col<T> op() { return RR<T>::nuisance; }
    void calculate(bool target = true,
		   bool nuisance = true,
		   bool propensity = false) override;
  };


  ////////////////////////////////////////////////////////////////////////////////

  class ACE : public Target<Complex> {
  protected:
    bool binary;

  public:
    ACE(const arma::cx_vec &y,
  	const arma::cx_mat &a,
  	const arma::cx_mat &x2,
  	const arma::cx_mat &x3,
  	const arma::cx_vec &parameter,
  	const arma::cx_vec &weights,
	bool binary = true);

    void calculate(bool target = true, bool nuisance = true, bool propensity = true);

    arma::cx_mat est(arma::cx_vec par,
		     bool indiv = false,
		     const Complex &value = 1);
    arma::cx_mat est(bool indiv = false,
		     const Complex &value = 1);
    arma::mat deriv(const Complex &value = 1);
  };


  ////////////////////////////////////////////////////////////////////////////////

  template<typename T>
  arma::Mat<T> rd2prob(const arma::Col<T> &rd, const arma::Col<T> &op);

  template<typename T>
  arma::Mat<T> rr2prob(const arma::Col<T> &rd, const arma::Col<T> &op);

}  // namespace target

#endif  // SRC_TARGET_H_
