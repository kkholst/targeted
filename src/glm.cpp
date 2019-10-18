/*!
  @file glm.cpp
  @author Klaus K. Holst
  @copyright 2018, Klaus Kähler Holst

  @brief Utility functions for Generalized Linear Models

  Includes implementation of risk-difference estimator with nuisance
  model for log odds-product (MLE and DR estimator).

*/

#include "glm.hpp"

namespace glm {

  template arma::mat expit<double>(const arma::mat&);
  template arma::cx_mat expit<Complex>(const arma::cx_mat&);


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

}  // namespace glm



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
