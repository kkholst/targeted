/*!
  @file cumres.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Generic function for calculating cumulative residuals

  Test statistics

*/
#include <target/cumres.hpp>

namespace target {

  /*!
  Constructor for the cumres class.

  @param r column vector of residuals
  @param dr matrix of partial deriatives of the residuals wrt to the parameter vector
  @param ic matrix with the estimated influence functions for the parametric model
  */
  cumres::cumres(const arma::vec &r,
		 const arma::mat &dr,
		 const arma::mat &ic)
    : r(r), dr(dr), ic(ic) {
#ifndef ARMA_R
    arma::arma_rng::set_seed_random();
#endif
    n = r.n_elem;
    arma::vec inp(n, 1);
    for (unsigned i=0; i < n; i++) inp(i) = i;
    this->ord = arma::conv_to<arma::uvec>::from(inp);
    this->inp = inp;
    this->order(inp);
    this->b = arma::vec();
  }

  void cumres::order(const arma::mat &inp, arma::vec b) {
    unsigned p = inp.n_cols;
    arma::umat ord(inp.n_rows, p);
    this->inp = arma::mat(inp.n_rows, p);
    for (unsigned j=0; j < p; j++) {
      arma::vec inpj = inp.col(j);
      ord.col(j) = arma::stable_sort_index(inpj);
      this->inp.col(j) = inpj.elem(ord.col(j));
    }
    this->ord = ord;
    if (p == 1 && b.n_elem==0) {
      eta = arma::cumsum(dr.rows(ord), 0);
    }
    if (b.n_elem>0) {
      if (b.n_elem<p) {
	arma::vec newb = arma::vec(p);
	for (unsigned i=0; i<p; i++) newb(i) = b(0);
	b = newb;
      }
      this->b = b;
    }
  }

  /*!
    Sample n independent standard normal distributed variables.
  */
  arma::vec cumres::rnorm() {
#ifdef ARMA_R
    Rcpp::RNGScope scope;
    return Rcpp::as<arma::vec>(Rcpp::rnorm(n));
#else
    return arma::randn<arma::vec>(n);
#endif
  }


  /*!
    Calculate the observed cumulative residual process

    \f[ W(t) = n^{-1/2}\sum_{i=1}^n 1\{t-b<X_i\leq t\}r_i, \f]

    where \f$r_i\f$ is the the residual corresponding to the \f$i\f$th
    observation and \f$X_i\f$ is the variable which the process is
    ordered against (as defined by the \c inp argument to cumres::order).

    When \c b is not set (i.e., an empty vector) the standard
    cumulative residual process is calculated (corresponding to \f$b=\infty\f$):

    \f[ W(t) = n^{-1/2}\sum_{i=1}^n 1\{X_i\leq t\}r_i.\f]

  */
  arma::mat cumres::obs() {
    arma::mat res(n, inp.n_cols);
    double scale = std::sqrt(static_cast<double>(n));
    for (unsigned j=0; j < inp.n_cols; j++) {
      if (b.n_elem>0) { // Moving average
	arma::vec ro = r.elem(ord.col(j));
	arma::vec rj = arma::cumsum(ro);
	unsigned pos = 0;
	double lval = 0.0;
	for (unsigned i=0; i < ro.n_elem; i++) {
	  if (inp(i)-b(j)<lval)
	  while (inp(pos) >= inp(i)-b(j)) pos++;
	  res(i,j) = (rj(i) - rj(pos)) / scale;
	}
      } else { // Standard cumulative sum
	arma::vec rj = arma::cumsum(r.elem(ord.col(j))) / scale;
	res.col(j) = rj;
      }
    }
    return res;
  }

  /*!
    Obtain a single sample of the residual process under the null hypothesis (true model).

    @param idx indices in which to evaluate the process.
               If this is an empty vector the process is evaluated in all observed points.
  */
  arma::mat cumres::sample(const arma::umat &idx) {
    arma::vec g = rnorm();
    unsigned N = n;
    unsigned p = ord.n_cols;
    if (!idx.is_empty()) {
	N = idx.n_rows;
    }
    arma::mat res(N, p);
    for (unsigned i=0; i < p; i++) {
      arma::uvec curord = ord.col(i);
      arma::vec w1 = arma::cumsum(r.elem(curord)%g);
      if (p > 1) {
	// Cumulative derivative of residuals (cumsum for each column):
	eta = arma::cumsum(dr.rows(curord), 0);
      }
      arma::rowvec B(ic.n_cols);
      for (unsigned j=0; j < ic.n_cols; j++) {
	arma::vec tmp = ic.col(j);
       	B(j) = sum(tmp.elem(curord)%g);
      }
      arma::vec w2(N);
      if (!idx.is_empty()) {
	w1 = w1.elem(idx.col(i));
	for (unsigned j=0; j < N; j++) {
	  w2(j) = arma::as_scalar(B*eta.row(idx(j, i)).t());
	}
      } else {
	for (unsigned j=0; j < n; j++) {
	  w2(j) = arma::as_scalar(B*eta.row(j).t());
	}
      }
      res.col(i) = (w1+w2)/std::sqrt(static_cast<double>(n));
    }

    return res;
  }

  /*!
    Draw R samples from the cumulative residual process under the null hypothesis (true model)

    @param R Number of process to sample
    @param idx subset of indices to evalute the process in
    @param quantiles Boolean that defines whether quantiles of the sampled process is to be estimated
    @return arma::mat \f$R\times 2p\f$ matrix with Supremum and L2 test statistics for each of the \f$p\f$ variables (columns in the \c inp variable defined in cumres::order)
   */
  arma::mat cumres::sample(unsigned R,             // Number of samples
			   const arma::umat &idx,
			   bool quantiles) {
    unsigned p = ord.n_cols;
    arma::mat res(R, 2*p);
    // qt.fill(0);
    // unsigned n = this->n;
    // arma::mat qt(n, std::ceil(R*0.05));
    for (unsigned i=0; i < R; i++) {
      arma::mat wi = this->sample(idx);
      for (unsigned j=0; j < p; j++) {
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
