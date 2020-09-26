/*!
  @file nb.hpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Weighted Naive Bayes

*/

#include <cmath>
#include "target/nb.hpp"

namespace target {

std::vector<raggedArray> nb(arma::vec  y,
	       arma::mat  x,
	       arma::uvec xlev,
	       arma::vec  ylev,
	       arma::vec  weights,
	       double     laplacesmooth) {

  if (ylev.n_elem == 0) {
    ylev = arma::unique(y);
  }
  unsigned K = ylev.n_elem;
  unsigned n = y.n_elem;
  unsigned p = x.n_cols;
  if (xlev.n_elem < p) {
    xlev = arma::uvec(p);
    for (unsigned j=0; j<p; j++) xlev(j) = j;
  }
  if (weights.n_elem < n) {
    weights = arma::vec(n);
    for (unsigned i=0; i<n; i++) weights(i) = 1;
  }

  std::vector<raggedArray> res(K);
  for (unsigned k=0; k<K; k++) {
    unsigned N = 0;
    for (unsigned i=0; i<n; i++) {
      if (y[i]==ylev[k]) N++;
    }
    arma::uvec idx(N);
    N = 0;
    for (unsigned i=0; i<n; i++) {
      if (y[i]==ylev[k]) {
	idx(N) = i;
	N++;
      }
    }
    res[k] = pcond(idx,x,xlev,weights,laplacesmooth);
  }
  return res;
}


raggedArray pcond(const arma::uvec   &idx,
		  const arma::mat    &x,
		  const arma::uvec   &xlev,
		  const arma::vec    &weights,
		  double              laplacesmooth) {
  unsigned p = x.n_cols;
  raggedArray val(p);
  double sw = 0;
  for (unsigned i=0; i<idx.n_elem; i++) sw += weights(i);

  for (unsigned j=0; j<p; j++) {
    if (xlev(j)>0) { // factor
      arma::vec est(xlev[j]);
      for (unsigned i=0; i<idx.n_elem; i++) {
	double xval = x(idx(i),j);
	est((int)xval) += weights(idx(i));
      }
      double s = 0;
      if (laplacesmooth>0)
	for (unsigned i=0; i<est.n_elem; i++) est(i) += laplacesmooth;
      for (unsigned i=0; i<est.n_elem; i++) s += est(i);
      for (unsigned i=0; i<est.n_elem; i++) est(i) /= s;
      val[j] = est;
    } else {
      double m=0, ss=0;
      arma::vec est(2);
      for (unsigned i=0; i<idx.n_elem; i++) {
	double xval = x(idx(i), j);
	double w = weights(idx(i))/sw;
	double xw = xval*w;
	m += xw;
	ss += xval*xw;
      }
      if (idx.n_elem == 1) {
	est(1) = 0;
      }  else {
	est(1) = std::sqrt((ss-m*m)*sw/(sw-1));
      }
      est(0) = m;
      val[j] = est;
    }
  }
  return val;
}


arma::mat prednb(const arma::mat &X,
		 const raggedArray &condprob,
		 raggedArray xord,
		 arma::uvec multinomial,
		 arma::vec prior,
		 double threshold) {
  return X;
}

//   unsigned p = X.n_cols;
//   unsigned n = X.n_rows;
//   unsigned nclasses = condprob.size()/p;
//   arma::mat lposterior(n,nclasses);
//   arma::vec px(n); px.fill(0);

//   for (unsigned k=0; k<nclasses; k++) {
//     arma::vec lpcond(n); lpcond.fill(0);
//     for (unsigned j=0; j<p; j++) {
//       unsigned pos = k*p + j;
//       NumericVector estx = condprob[pos];
//       if (multinomial[j]) {
// 	arma::uvec x = arma::conv_to<arma::uvec>::from(X.col(j));
// 	NumericVector xx = xord(j);
// 	NumericVector est = clone(xx);
// 	LogicalVector estna = is_na(est);
// 	for (unsigned i=0; i<est.size(); i++)
//        	  if (estna[i]) {
// 	    est[i] = threshold;
// 	  } else {
// 	    est[i] = estx[est[i]];
// 	    if (est[i]<threshold) est[i] = threshold;
// 	  }
// 	double s = sum(est);
// 	arma::vec logest(est.size());
// 	for (unsigned i=0; i<est.size(); i++) {
// 	  logest[i] = log(est[i]/s);
//        	}
// 	lpcond += logest.elem(x);
//       } else { // Gaussian
// 	arma::vec x = X.col(j);
//       	LogicalVector estna = is_na(estx);
//       	if (estna[0]) estx[0] = 0;
//       	if (estna[1] | (estx[1]<1E-16)) estx[1] = 1;
//       	for (unsigned i=0; i<n; i++) {
//       	  lpcond[i] += Rf_dnorm4(x[i],estx[0],estx[1],true);
// 	}
//       }
//     }
//     lposterior.col(k) = lpcond + std::log(prior[k]); // log P(x,c)
//     px = px + exp(lposterior.col(k)); // P(x)
//   }
//   arma::colvec lpx = -log(px);
//   lposterior.each_col() += lpx; // log P(c|x)
//   return lposterior;
// }


} // namespace target
