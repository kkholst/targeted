/*!
  @file riskreg_interface.cpp
  @author Klaus K. Holst
  @copyright 2018-2020, Klaus Kähler Holst

  @brief R interface for the TargetedBinary subclasses. 

  The relevant bindings are created in \c RcppExports.cpp, \c RcppExports.h
  with \c Rcpp::compileAttributes()
*/

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <target/riskreg.hpp>
#include <cmath>
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)
#include <cfloat>     // precision of double (DBL_MIN)
#include <functional> // std::bind for using non-static member function as argument to free function

using cx_dbl = target::cx_dbl;
using cx_func = target::cx_func;

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
    target::RD<cx_dbl> inp(y, a, x1, x2, x2, par, weights);
    res = inp.score(indiv);
  } else {
    target::RR<cx_dbl> inp(y, a, x1, x2, x2, par, weights);
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
    target::RD<cx_dbl> inp(y, a, x1, x2, x3, par, weights);
    res = inp.est(alpha);
  } else {
    target::RR<cx_dbl> inp(y, a, x1, x2, x3, par, weights);
    res = inp.est(alpha);
  }
  return ( res );
}



// [[Rcpp::export]]
Rcpp::List ace_est(const arma::vec &y,
		   const arma::mat &a,
		   const arma::mat &x1,
		   const arma::mat &x2,
		   const arma::vec &theta,
		   const arma::vec &weights,
		   bool binary = true) {
  arma::vec par(theta.n_elem+1);
  par[0] = 0;
  for (unsigned i=0; i < theta.n_elem; i++) par[i+1] = theta[i];
  target::ACE model(y, a, x1, x2, par, weights, binary);
  double alpha = real(model.est(false)[0])/y.n_elem;
  par[0] = alpha;
  model.update_par(par);
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
    res = target::logistic_iid(y, p, x1, weights).iid;
  } else {
    res = target::linear_iid(y, p, x1, weights).iid;
  }
  return res;
}



RCPP_MODULE(riskregmodel){
    using namespace Rcpp ;
    class_<RiskReg>("RiskReg")
    // expose the constructor
      .constructor<arma::vec , arma::vec ,
		   arma::mat , arma::mat , arma::mat ,
		   arma::vec , std::string>("Constructor")
      
      .method("logl",   &RiskReg::logl,   "log-likelihood")
      .method("dlogl",  &RiskReg::dlogl,  "score function")
      .method("update", &RiskReg::update, "Update parameter vector")
      ;
}

