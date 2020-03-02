/*!
  @file target.cpp
  @author Klaus K. Holst
  @copyright 2019-2020, Klaus Kähler Holst

  @brief Classes for targeted inference models

  Includes implementation of risk-difference estimator with nuisance
  model for log odds-product (MLE and DR estimator).

*/

#include <limits>
#include "target.hpp"
#include "glm.hpp"

namespace target {
  
  // typedef Target<T> B;

  /*!
    @brief Target class constructur

    @param y - Response vector
    @param a - Exposure/treatment vector
    @param x1 - Design matrix for target model
    @param x2 - Design matrix for nuisance model
    @param x3 - Design matrix for exposure propensity
    @param parameter - Parameter vector (order: \c target, \c nuisance, and optionally \c propensity)
    @param weights - Optional weights
    @return Target class
  */
  template <typename T>
  Target<T>::Target(const arma::Col<T> &y,
		    const arma::Mat<T> &a,
		    const arma::Mat<T> &x1,
		    const arma::Mat<T> &x2,
		    const arma::Mat<T> &x3,
		    const arma::Col<T> &parameter,
		    const arma::Col<T> &weights) : Target(y, a, x1, x2, x3, parameter) {
    this->weights(weights);
  }


  /*!
    @brief update_par -

    @param parameter - Description of parameter
  */
  template <typename T>
  void Target<T>::update_par(const arma::Col<T> &parameter) {
    unsigned pos = 0;
    for (unsigned i=0; i < alpha.n_elem; i++) {
      alpha[i] = parameter[i];
    }
    pos += alpha.n_elem;
    for (unsigned i=0; i < beta.n_elem; i++) {
      beta[i] = parameter[i+pos];
    }
    pos += beta.n_elem;
    if (parameter.n_elem == pos+gamma.n_elem) {
      for (unsigned i=0; i < gamma.n_elem; i++) {
	gamma[i] = parameter[i+pos];
      }
    }
  }


  template <typename T>
  Target<T>::Target(const arma::Col<T> &y,
		    const arma::Mat<T> &a,
		    const arma::Mat<T> &x1,
		    const arma::Mat<T> &x2,
		    const arma::Mat<T> &x3,
		    const arma::Col<T> &parameter) {
    this->update_data(y,a,x1,x2,x3);
    alpha = arma::Col<T>(x1.n_cols);
    beta = arma::Col<T>(x2.n_cols);
    gamma = arma::Col<T>(x3.n_cols);
    update_par(parameter);
  }

  template <typename T>
  Target<T>::Target(const arma::Col<T> &y,
			const arma::Mat<T> &a,
			const arma::Mat<T> &x1,
			const arma::Mat<T> &x2,
			const arma::Col<T> &parameter,
			const arma::Col<T> &weights) : Target(y, a, x1, x2, x2, parameter) {
    this->weights(weights);    
  }

  template <typename T>
  Target<T>::Target(const arma::Col<T> &y,
			const arma::Mat<T> &a,
			const arma::Mat<T> &x1,
			const arma::Mat<T> &x2,
			const arma::Col<T> &parameter) : Target(y, a, x1, x2, x2, parameter)  {
  }



  template <typename T>
  void Target<T>::calculate(bool target, bool nuisance, bool propensity) {
    if (target)  // Target, linear predictor
      this->target = Target<T>::X1()*Target<T>::alpha;
    arma::Mat<T> tt = Target<T>::X2()*Target<T>::beta;
    if (nuisance)  // Nuisance, linear predictor
      this->nuisance = Target<T>::X2()*Target<T>::beta; 
    if (propensity && Target<T>::gamma.n_elem > 0) {  // Propensity
      this->propensity = Target<T>::X3()*Target<T>::gamma;
      this->propensity = target::expit(this->propensity);
    }
  }

  ////////////////////////////////////////////////////////////////////////////////


  /*!
    @brief Calculate risk probabilities conditional on exposure

    \f$p_a(V) = E(Y \mid A=a, V), a\in\{0,1\}\f$

    \todo write more
    @return arma::Mat
  */
  template <typename T>
  arma::Mat<T> TargetBinary<T>::pa() {
    arma::Mat<T> res(this->Y().n_elem, 5);
    res.col(1) = this->p(0);
    res.col(2) = this->p(1);
    res.col(0) = (1-this->A())%res.col(1) + this->A()%res.col(2);
    res.col(3) = this->target;
    res.col(4) = this->nuisance;
    return(res);
  }
 
  /*!
    @brief log likelihood

    @param indiv - if \c true return individual log likelihood contributions
    @return vector of type \c arma::Vec<T>
  */
  template <typename T>
  arma::Col<T> TargetBinary<T>::loglik(bool indiv) {
    arma::Col<T> phat = TargetBinary<T>::p(0) %
      (1-Target<T>::A()) + TargetBinary<T>::p(1) % Target<T>::A();
    arma::Col<T> logl = Target<T>::Y() % log(phat) +
      (1-Target<T>::Y()) % log(1-phat);
    logl %= Target<T>::weights();
    if (indiv) return(logl);
    return(sum(logl, 0));
  }


  /*!
    @brief Estimating function for double robust estimator

    \f[
    U(\alpha; \widehat{\alpha}, \widehat{\beta}, \widehat{\gamma}) =
    \omega(V)\Big(A-e(V;\widehat{\gamma})\Big)\left (H(\alpha)-p_{0}(V;\widehat{\alpha},\widehat{\beta})\right)
    \f]

    Note that
    1. Inital estimates \f$\widehat{\alpha},\widehat{\beta}\f$ obtained from MLE.
    2. Similarly, \f$\widehat{\gamma}\f$ obtained from regular asymptotic linear model (e.g., logistic regression MLE).
    3. Plugin estimates to obtain \f$\widehat{\omega}_{\mathrm{eff}}\f$. Also, note that \f$p_0\f$ is calculated wrt initial MLE.

    @param alpha target parameter
    @param propensity exposure propensity weights
    @return arma::Mat<T>
  */
  template <typename T>
  arma::Mat<T> TargetBinary<T>::est(arma::Col<T> alpha,
				      const arma::Col<T> &propensity) {
    arma::Col<T> p0 = this->p(0);
    for (unsigned i=0; i < alpha.n_elem; i++) this->alpha[i] = alpha[i];
    calculate(true, false, false);
    // arma::Col<T> a = this->A().col(0);
    arma::Col<T> H = this->H();
    arma::Col<T> S = (this->A()-propensity)%(H-p0);
    S %= this->weights();
    arma::Mat<T> U(S.n_elem, alpha.n_elem);
    for (unsigned i=0; i < alpha.n_elem; i++) {
      U.col(i) = S % this->X1().col(i);
    }
    return(U);
  }

  template <typename T>
  arma::Mat<T> TargetBinary<T>::est(arma::Col<T> alpha) {
    calculate(false, false, true);  // Calculate propensity weights
    return( sum(TargetBinary<T>::est(alpha, this->propensity), 0) );
  }


  /*!
    @brief score -

    @param indiv - Description of indiv
    @return arma::Mat
  */
  template <typename T>
  arma::Mat<T> TargetBinary<T>::score(bool indiv) {
    arma::Col<T> phat = this->p(0) % (1-this->A().col(0)) +
      this->p(1) % this->A().col(0);
    arma::Mat<T> dp_dlp = dp();
    arma::Col<T> S = (this->Y()-phat) / (phat%(1-phat));
    S %= this->weights();
    for (unsigned i=0; i < dp_dlp.n_cols; i++)
      dp_dlp.col(i) %= S;
    if (indiv) return(dp_dlp);
    return(sum(dp_dlp, 0));
  }


  template <typename T>
  void TargetBinary<T>::calculate(bool target,
				  bool nuisance,
				  bool propensity) {
    Target<T>::calculate(target, nuisance, propensity);
    if (nuisance)
      this->nuisance = exp(this->nuisance);  // Odds-product
  }

  ////////////////////////////////////////////////////////////////////////////////

  template <typename T>
  RR<T>::RR(const arma::Col<T> &y,
	    const arma::Mat<T> &a,
	    const arma::Mat<T> &x1,
	    const arma::Mat<T> &x2,
	    const arma::Mat<T> &x3,
	    const arma::Col<T> &parameter,
	    const arma::Col<T> &weights) :
    TargetBinary<T>(y, a, x1, x2, x3, parameter, weights) {
    calculate();
  }
  
  template <typename T>
  RD<T>::RD(const arma::Col<T> &y,
	    const arma::Mat<T> &a,
	    const arma::Mat<T> &x1,
	    const arma::Mat<T> &x2,
	    const arma::Mat<T> &x3,
	    const arma::Col<T> &parameter,
	    const arma::Col<T> &weights) :
    TargetBinary<T>(y, a, x1, x2, x3, parameter, weights) {
    calculate();
  }

  template <typename T>
  void RD<T>::calculate(bool target, bool nuisance, bool propensity) {
    TargetBinary<T>::calculate(target, nuisance, propensity);
    if (target)
      this->target = tanh(this->target);  // Relative risk
    if (target || nuisance)
      this->pr = rd2prob(this->target, this->nuisance);
  }

  template <typename T>
  void RR<T>::calculate(bool target, bool nuisance, bool propensity) {
    TargetBinary<T>::calculate(target, nuisance, propensity);
    if (target)
      this->target = exp(this->target);  // Relative risk
    if (target || nuisance)
      this->pr = rr2prob(this->target, this->nuisance);
  }

  template <typename T>
  arma::Mat<T> RD<T>::dp() {
    arma::Col<T> a = RD<T>::A().col(0);
    arma::Col<T> s0 = RD<T>::p(0) % (1 - RD<T>::p(0));
    arma::Col<T> s1 = RD<T>::p(1) % (1 - RD<T>::p(1));
    arma::Col<T> dp0_rd = -s0/(s0 + s1);
    arma::Col<T> d0 = (dp0_rd + a) % (1 - rd()%rd());
    arma::Mat<T> d_alpha = RD<T>::X1();
    for (unsigned i=0; i < d_alpha.n_cols; i++) d_alpha.col(i) %= d0;
    arma::Col<T> dp0_op = s0 % s1/(s0 + s1);
    arma::Mat<T> d_beta = RD<T>::X2();
    for (unsigned i=0; i < d_beta.n_cols; i++) d_beta.col(i) %= dp0_op;
    return ( join_rows(d_alpha, d_beta) );
  }

  template <typename T>
  arma::Mat<T> RR<T>::dp() {
    arma::Col<T> a = RR<T>::A().col(0);
    arma::Col<T> s = (1 - RR<T>::p(0)) + (1 - RR<T>::p(1));
    arma::Col<T> tmp = 1 - a + a%rr();
    arma::Col<T> dp_rr = -RR<T>::p(0)/RR<T>::p(1) %
      RR<T>::p(0)%(1-RR<T>::p(0)) / s % tmp + a % RR<T>::p(0);
    dp_rr %= rr();
    arma::Mat<T> d_alpha = RR<T>::X1();
    for (unsigned i=0; i < d_alpha.n_cols; i++) d_alpha.col(i) %= dp_rr;
    s = (1-RR<T>::p(1))%RR<T>::p(1) + (1-RR<T>::p(0))%RR<T>::p(1);
    arma::Col<T> dp_op = pow((1-RR<T>::p(0))%(1-RR<T>::p(1)), 2) / s % tmp;
    arma::Mat<T> d_beta = RR<T>::X2();
    dp_op %= op();
    for (unsigned i=0; i < d_beta.n_cols; i++) d_beta.col(i) %= dp_op;
    return ( join_rows(d_alpha, d_beta) );
  }

  /*!
    @brief rd2prob - Computes risk probabilities given exposure 0 or 1.

    @param rd - Risk difference vector
    @param op - Odds-product vetor
    @return arma::Mat<T> with two columns with probabilities given exposure 0 or 1
  */
  template <typename T>
  arma::Mat<T> rd2prob(const arma::Col<T> &rd, const arma::Col<T> &op) {
    arma::Col<T> a = op-1;
    arma::Col<T> b = -op%(2-rd)-rd;
    arma::Col<T> p0 = (-b - sqrt(b%b - 4*op%(1.0 - rd)%a)) / (2*a);
    for (unsigned i=0; i < p0.size(); i++) {
      if (std::abs(op[i]-1.0) < 1e-16) p0[i] = 0.5*(1.0 - rd[i]);
    }
    arma::Mat<T> res(rd.n_elem, 2);
    res.col(0) = p0;
    res.col(1) = p0+rd;
    return res;
  }

  /*!
    @brief rr2prob - Computes risk probabilities given exposure 0 or 1.

    @param rr - Relative risk vector
    @param op - Odds-product vetor
    @return arma::Mat<T> with two columns with probabilities given exposure 0 or 1
  */
  template <typename T>
  arma::Mat<T> rr2prob(const arma::Col<T> &rr, const arma::Col<T> &op) {
    arma::Mat<T> res(rr.n_elem, 2);
    arma::Col<T> a = rr%(1.0 - op);
    arma::Col<T> b = op%(1.0 + rr);
    arma::Col<T> p0 = (-b + sqrt(b%b + 4*a%op))/(2*a);
    for (unsigned i=0; i < p0.size(); i++) {
      if (std::abs(op[i]-1.0) < 1e-16) p0[i] = (T)1/(1.0 + rr[i]);
    }
    res.col(0) = p0;
    res.col(1) = p0%rr;
    return res;
  }

  ////////////////////////////////////////////////////////////////////////////////


  /*!
    @brief ACE AIPW for Average Causal Effects

    Assume we have iid observations \f$(Y_i,A_i,X_i)\f$
    \f[
    E(Y^\ast) = \sum_{i=1}^n 
    \f]
    @param y Response variable
    @param a Exposure variable
    @param x2 Design matrix for outcome regression. Note must be calculated for fixed A=a (to derive distribution of potential outcome Y^*(a))
    @param x3 Design matrix for propensity model
    @param parameter Parameters from outcome regression and propensity model
    @param weights Weight vector
    @param binary If TRUE outcome regression is assumed to be a logistic regression model.
    @return ACE
  */
  ACE::ACE(const arma::cx_vec &y,
	   const arma::cx_mat &a,
	   const arma::cx_mat &x2,
	   const arma::cx_mat &x3,
	   const arma::cx_vec &parameter,
	   const arma::cx_vec &weights,
	   bool binary) :
    Target<cx_dbl>(y, a, arma::cx_mat(1, 1), x2, x3, parameter, weights) {
    this->binary = binary;
    ACE::calculate(true, true);
  }

  ACE::ACE(const arma::vec &y,
  	   const arma::vec &a,
  	   const arma::mat &x2,
  	   const arma::mat &x3,
  	   const arma::vec &parameter,
  	   const arma::vec &weights,
  	   bool binary) :
    ACE(arma::conv_to<arma::cx_vec>::from(y),
  	     arma::conv_to<arma::cx_vec>::from(a),
  	     arma::conv_to<arma::cx_mat>::from(x2),
  	     arma::conv_to<arma::cx_mat>::from(x3),
  	     arma::conv_to<arma::cx_vec>::from(parameter),
  	     arma::conv_to<arma::cx_vec>::from(weights),
  	     binary) { }
  
  void ACE::calculate(bool target, bool nuisance, bool propensity) {
    Target<cx_dbl>::calculate(false, nuisance, propensity);
    if (nuisance && this->binary) {
      this->nuisance = target::expit(this->nuisance);  // Outcome regression model
    }
  }

  arma::cx_mat ACE::est(bool indiv, const cx_dbl &value) {
    arma::cx_vec a1 = A().as_col();
    for (unsigned i=0; i < a1.n_elem; i++) a1[i] = (a1[i] == value);
    arma::cx_vec U = Y()%a1/(propensity) -
      (a1-propensity)%nuisance/(propensity) - alpha[0];
    U %= weights();
    if (indiv) return( std::move(U) );
    return(sum(U, 0));
  }

  arma::cx_mat ACE::est(arma::cx_vec par, bool indiv, const cx_dbl &value) {
    Target<cx_dbl>::update_par(par);
    ACE::calculate();
    return(ACE::est(indiv, value));
  }

  void ACE::update_par(arma::cx_vec par) {
    this->Target<cx_dbl>::update_par(par);
  }
  
  void ACE::update_par(arma::vec par) {
    arma::cx_vec parc = arma::conv_to<arma::cx_vec>::from(par);
    this->Target<cx_dbl>::update_par(parc);
  }

  /*!
    @brief deriv Calculate derivative of estimating function for AIPW estimator

    Complex step derivative.
    @param value Matrix with partial derivatives of estimating function
    @return arma::mat
  */
  arma::mat ACE::deriv(const cx_dbl &value) {
    unsigned n1 = this->alpha.n_elem;
    unsigned n2 = this->beta.n_elem;
    unsigned n3 = this->gamma.n_elem;
    unsigned p = n1+n2+n3;
    arma::vec res(p);
    // int n = Y().n_elem;
    res[0] = -sum(real(weights()));
    double eps = std::numeric_limits<float>::denorm_min();
    cx_dbl delta(0, eps);
    unsigned pos = 1;
    cx_dbl tmp;
    
    for (unsigned i=0; i < n2; i++) {
      tmp = this->beta[i];
      this->beta[i] += delta;
      ACE::calculate(true, true, false);
      this->beta[i] = tmp;
      res[i+pos] = std::imag(ACE::est(false, value)[0])/eps;
    }
    pos += n2;
    for (unsigned i=0; i < n3; i++) {
      tmp = this->gamma[i];
      this->gamma[i] += delta;
      ACE::calculate(i == 0, true);
      this->gamma[i] = tmp;
      res[i+pos] = std::imag(ACE::est(false, value)[0])/eps;
    }
    return( std::move(res) );
  }


  ////////////////////////////////////////////////////////////////////////////////

  
  // specific template instantiations:
  template class RR<double>;
  template class RR<cx_dbl>; 
  template class RD<double>;
  template class RD<cx_dbl>;
  template class Target<double>;
  template class Target<cx_dbl>;  
  template class TargetBinary<double>;
  template class TargetBinary<cx_dbl>;
  
 

}  // namespace target
