/*!
  @file rinterface.cpp
  @author Klaus K. Holst
  @copyright 2018, Klaus Kähler Holst

  @brief R interface for the TargetedBinary subclasses. 

  The relevant bindings are created in \c RcppExports.cpp, \c RcppExports.h
  \c Rcpp::compileAttributes()
*/

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <Rmath.h>
#include <cmath>
#include "target.h"
#include "glm.h"

typedef std::complex<double> Complex;

// [[Rcpp::export]]
arma::vec bin_logl(const arma::vec &y,
		   const arma::vec &a,
		   const arma::mat &x1,
		   const arma::mat &x2,
		   const arma::vec par,
		   const arma::vec &weights,
		   std::string type = "rd",
		   bool indiv = false) {
  arma::vec res;
  if (!type.compare("rd")) {
    target::RD<double> inp(y, a, x1, x2, x2, par, weights);
    res = inp.loglik(indiv);
  } else {
    target::RR<double> inp(y, a, x1, x2, x2, par, weights);
    res = inp.loglik(indiv);
  }
  return( res );
}


// [[Rcpp::export]]
arma::mat bin_dlogl(const arma::vec &y,
		    const arma::vec &a,
		    const arma::mat &x1,
		    const arma::mat &x2,
		    const arma::vec par,
		    const arma::vec &weights,
		    std::string type = "rd",
		    bool indiv = false) {
  arma::mat res;
  if (!type.compare("rd")) {
    target::RD<double> inp(y, a, x1, x2, x2, par, weights);
    res = inp.score(indiv);
  } else {
    target::RR<double> inp(y, a, x1, x2, x2, par, weights);
    res = inp.score(indiv);
  }
  return( res );
}

// [[Rcpp::export]]
arma::mat bin_pa(const arma::vec &y,
		 const arma::vec &a,
		 const arma::mat &x1,
		 const arma::mat &x2,
		 const arma::vec par,
		 std::string type = "rd") {
  arma::mat res;
  if (!type.compare("rd")) {
    target::RD<double> inp(y, a, x1, x2, x2, par, a);
    res = inp.pa();
  } else {
    target::RR<double> inp(y, a, x1, x2, x2, par, a);
    res = inp.pa();
  }
  return( res );
}



// [[Rcpp::export]]
arma::cx_mat bin_dlogl_c(const arma::cx_vec &y,
			 const arma::cx_vec &a,
			 const arma::cx_mat &x1,
			 const arma::cx_mat &x2,
			 const arma::cx_vec par,
			 const arma::cx_vec &weights,
			 std::string type = "rd",
			 bool indiv = false) {
  arma::cx_mat res;
  if (!type.compare("rd")) {
    target::RD<Complex> inp(y, a, x1, x2, x2, par, weights);
    res = inp.score(indiv);
  } else {
    target::RR<Complex> inp(y, a, x1, x2, x2, par, weights);
    res = inp.score(indiv);
  }
  return( res );
}


// [[Rcpp::export]]
arma::mat bin_esteq(const arma::vec &y,
		    const arma::vec &a,
		    const arma::mat &x1,
		    const arma::mat &x2,
		    const arma::vec &pr,
		    arma::vec alpha,
		    arma::vec par,
		    const arma::vec &weights,
		    std::string type = "rd") {
  arma::mat res;
  if (!type.compare("rd")) {
    target::RD<double> inp(y, a, x1, x2, x2, par, weights);
    res = inp.est(alpha, pr);
  } else {
    target::RR<double> inp(y, a, x1, x2, x2, par, weights);
    res = inp.est(alpha, pr);
  }
  return ( res );
}

// [[Rcpp::export]]
arma::cx_mat bin_esteq_c(const arma::cx_vec &y,
			 const arma::cx_vec &a,
			 const arma::cx_mat &x1,
			 const arma::cx_mat &x2,
			 const arma::cx_mat &x3,
			 arma::cx_vec alpha,
			 arma::cx_vec par,
			 const arma::cx_vec &weights,
			 std::string type = "rd") {
  arma::cx_mat res;
  if (!type.compare("rd")) {
    target::RD<Complex> inp(y, a, x1, x2, x3, par, weights);
    res = inp.est(alpha);
  } else {
    target::RR<Complex> inp(y, a, x1, x2, x3, par, weights);
    res = inp.est(alpha);
  }
  return ( res );
}



// [[Rcpp::export]]
Rcpp::List ace_est(const arma::cx_vec &y,
		   const arma::cx_vec &a,
		   const arma::cx_mat &x1,
		   const arma::cx_mat &x2,
		   const arma::cx_vec &theta,
		   const arma::cx_vec &weights,
		   bool binary = true) {
  arma::cx_vec par(theta.n_elem+1);
  par[0] = 0;
  for (unsigned i=0; i < theta.n_elem; i++) par[i+1] = theta[i];
  target::ACE model(y, a, x1, x2, par, weights, binary);
  double alpha = real(model.est(false)[0])/y.n_elem;
  par[0] = alpha;
  model.updatePar(par);
  model.calculate();
  arma::vec U = real(model.est(true));
  arma::mat dU = model.deriv();
  return ( Rcpp::List::create(Rcpp::Named("alpha") = alpha,
			      Rcpp::Named("u") = U,
			      Rcpp::Named("du") = dU) );
}


// [[Rcpp::export]]
arma::mat fast_iid(const arma::vec &y,
		   const arma::vec &p,
		   const arma::mat &x1,
		   const arma::vec &weights,
		   bool logistic = true) {
  arma::mat res;
  if (logistic) {
    res = glm::logistic_iid(y, p, x1, weights).iid;
  } else {
    res = glm::linear_iid(y, p, x1, weights).iid;
  }
  return res;
}


