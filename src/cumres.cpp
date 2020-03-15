/*!
  @file cumres.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Generic function for calculating cumulative residuals

  Test statistics 

*/

#include "cumres.hpp"

namespace target {

  cumres::cumres(const arma::vec &r, const arma::mat &dr, const arma::mat &ic) : r(r), dr(dr), ic(ic) {
#ifndef ARMA_R
    arma::arma_rng::set_seed_random();
#endif
    n = r.n_elem;
    arma::vec inp(n,1);
    for (unsigned i=0; i<n; i++) inp(i) = i;
    this->ord = arma::conv_to<arma::uvec>::from(inp);
    this->inp = inp;
    this->order(inp);
  }

  void cumres::order(const arma::mat &inp) {
    unsigned p = inp.n_cols;
    arma::umat ord(inp.n_rows, p);    
    this->inp = arma::mat(inp.n_rows, p);    
    for (unsigned j=0; j<p; j++) {
      arma::vec inpj = inp.col(j);
      ord.col(j) = arma::stable_sort_index(inpj);
      this->inp.col(j) = inpj.elem(ord.col(j));
    }
    this->ord = ord;
    if (p==1) {
      eta = arma::cumsum(dr.rows(ord), 0);
    }
  }

  arma::vec cumres::rnorm() {
#ifdef ARMA_R
    Rcpp::RNGScope scope;
    return Rcpp::as<arma::vec>(Rcpp::rnorm(n));
#else
    return arma::randn<arma::vec>(n);
#endif    
  }
  
  arma::mat cumres::obs() {
    arma::mat res(n, inp.n_cols);
    for (unsigned i=0; i<inp.n_cols; i++) {
      arma::vec ri = arma::cumsum(r.elem(ord.col(i)))/std::sqrt((double)n);
      res.col(i) = ri;
    }
    return res;
  }

  // Sample single process 
  arma::mat cumres::sample(const arma::umat &idx) {    
    arma::vec g = rnorm();
    unsigned N = n;
    unsigned p = ord.n_cols;
    if (!idx.is_empty()) {
	N = idx.n_rows;
    }
    arma::mat res(N, p);    
    for (unsigned i=0; i<p; i++) {
      arma::uvec curord = ord.col(i);
      arma::vec w1 = arma::cumsum(r.elem(curord)%g);
      if (p>1) {
	// Cumulative derivative of residuals (cumsum for each column):
	eta = arma::cumsum(dr.rows(curord), 0);
      }
      arma::rowvec B(ic.n_cols);
      for (unsigned j=0; j<ic.n_cols; j++) {
	arma::vec tmp = ic.col(j);
       	B(j) = sum(tmp.elem(curord)%g);
      }
      arma::vec w2(N);
      if (!idx.is_empty()) {
	w1 = w1.elem(idx.col(i));
	for (unsigned j=0; j<N; j++) {
	  w2(j) = arma::as_scalar(B*eta.row(idx(j,i)).t());
	}
      } else {
	for (unsigned j=0; j<n; j++) {
	  w2(j) = arma::as_scalar(B*eta.row(j).t());
	}
      }
      res.col(i) = (w1+w2)/std::sqrt((double)n);
    }
    
    return res;
  }

  // Sample 'r' processes
  arma::mat cumres::sample(unsigned R,     // Number of samples
			   const arma::umat &idx, // subset of 'time-points' of process to sample
			   bool quantiles) {

    unsigned p = ord.n_cols;
    arma::mat res(R, 2*p);
    // qt.fill(0);
    // unsigned n = this->n;
    // arma::mat qt(n, std::ceil(R*0.05));
    for (unsigned i=0; i<R; i++) {
      arma::mat wi = this->sample(idx);
      for (unsigned j=0; j<p; j++ ) {
	arma::vec t0 = inp.col(j);
	if (!idx.is_empty()) {
	  t0 = t0.elem(idx.col(j));
	}
	res(i, j*2) = SupTest(wi.col(j));	
	res(i, j*2+1) = L2Test(wi.col(j), t0);
      }
      /* if (quantiles) { // TODO: Disable for now. Capture quantiles
	 for (unsigned j=0; j<n; j++) {
	 wi = abs(wi);
	 arma::rowvec qtj = qt.row(j);
	 unsigned i = qtj.index_min();
	 if (wi(j)>qt(j,i)) qt(j,i) = wi(j);
	 }
	 } */
    }
    // this->qt = qt;
    return res;
  }

}  // namespace target

