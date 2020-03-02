/*!
  @file glm.cpp
  @author Klaus K. Holst
  @copyright 2018-2020, Klaus Kähler Holst

  @brief Utility functions for Generalized Linear Models

*/

#include "glm.hpp"

namespace target {

    // Softmax transformation using sum-log-exp trick to avoid overflow
  arma::mat softmax(arma::mat lp, bool ref=true, bool log=false) {
    if (ref) lp.insert_cols(0, arma::zeros(lp.n_rows));
    arma::colvec lpmax = arma::max(lp, 1);
    lp.each_col() -= lpmax;
    arma::colvec denom = sum(exp(lp), 1);
    lp.each_col() -= arma::log(denom);
    if (log) return(lp);
    return(exp(lp));
  }

  // Softmax transformation using sum-log-exp trick to avoid overflow
  arma::vec softmax(arma::vec u) {
    double umax = u.max();
    u -= umax;
    double denom = sum(exp(u));  
    return u - log(denom);
  }

  // template arma::mat expit<double>(const arma::mat&);
  // template arma::cx_mat expit<Complex>(const arma::cx_mat&);
  
  arma::mat expit(arma::mat x) {
    for (unsigned i=0; i<x.n_elem; i++) {
      double z = x(i);
      if (z>=0) {
	x(i) = 1/(1+exp(-z));
      } else {
	z = exp(z);
	x(i) = z/(1+z);
      }
    }
    return(x);
  }

  arma::cx_mat expit(arma::cx_mat x) {
    return 1.0/(1+exp(-x));
  }

  // template arma::mat expit<double>(const arma::mat&);
  // template arma::cx_mat expit<Complex>(const arma::cx_mat&);


  IID logistic_iid(const arma::vec &y,
		   const arma::vec &p,
		   const arma::mat &x,
		   const arma::vec &w) {
    arma::vec r = (y-p);
    arma::vec s = r%w;
    arma::mat U = x;
    for (unsigned i=0; i < x.n_cols; i++)
      U.col(i) %= s;
    arma::vec v = p%(1-p)%w;
    arma::mat H = arma::zeros(x.n_cols, x.n_cols);
    for (unsigned i=0; i < x.n_rows; i++) {
      H += v[i]*(trans(x.row(i))*x.row(i));
    }
    return IID(U, H.i());
  }

  IID linear_iid(const arma::vec &y,
		 const arma::vec &p,
		 const arma::mat &x,
		 const arma::vec &w) {
    arma::vec r = (y-p);
    double df = y.n_elem-x.n_cols;
    arma::vec s = r%w;
    double sigma2 = sum(r%s)/df;
    arma::mat U = x;
    for (unsigned i=0; i < x.n_cols; i++)
      U.col(i) %= s/sigma2;
    arma::mat H = arma::zeros(x.n_cols, x.n_cols);
    for (unsigned i=0; i < x.n_rows; i++) {
      H += w[i]*trans(x.row(i))*x.row(i);
    }
    return IID(U, sigma2*H.i());
  }

}  // namespace target



// arma::mat logistic_score(const arma::vec &beta,
// 			 const arma::vec &y,
// 			 const arma::mat &x,
// 			 const arma::vec &weights) {
//   arma::vec p = expit(x*beta);
//   arma::vec s = (y-p)%weights;
//   arma::vec res = x;
//   for (unsigned i=0; i < x.n_cols; i++)
//     res.col(i) %= s;
//   return res;
// }
// arma::mat logistic_vcov(const arma::vec &beta,
// 			const arma::mat &x,
// 			const arma::vec &weights) {
//   arma::vec p = expit(x*beta);
//   arma::vec v = p%(1-p)%weights;
//   arma::mat V = arma::zeros(beta.n_elem,beta.n_elem);
//   for (unsigned i=0; i < x.n_rows; i++) {
//     V += weights[i]*x.row(i)*x.row(i).t();
//   }
//   return V.i();
// }
// arma::mat linear_score(const arma::vec &beta,
// 		       const arma::vec &y,
// 		       const arma::mat &x,
// 		       const arma::vec &weights) {
//   arma::vec r = y-x*beta;
//   double df = y.n_elem-beta.n_elem;
//   arma::vec s = r%weights;
//   double sigma2 = sum(r%s)/df;
//   arma::vec res = x;
//   for (unsigned i=0; i < x.n_cols; i++)
//     res.col(i) %= s/sigma2;
//   return res;
// }
// arma::mat linear_vcov(const arma::vec &beta,
// 		      const arma::vec &y,
// 		      const arma::mat &x,
// 		      const arma::vec &weights) {
//   arma::vec r = y-x*beta;
//   double df = y.n_elem-beta.n_elem;
//   arma::vec s = r%weights;
//   double sigma2 = sum(r%s)/df;
//   arma::mat V = arma::zeros(beta.n_elem,beta.n_elem);
//   for (unsigned i=0; i < x.n_rows; i++) {
//     V += weights[i]*x.row(i)*x.row(i).t();
//   }
//   return( sigma2*V.i() );
// }
