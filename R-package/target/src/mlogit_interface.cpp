// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include "mlogit.hpp"
#include "utils.hpp"
#include <cmath>
#include <string>
#include <complex>
#include <memory>     // smart pointers (unique_ptr)
#include <cfloat>     // precision of double (DBL_MIN)
#include <functional> // std::bind for using non-static member function as argument to free function

using namespace Rcpp;
using namespace arma;


class MLogitR : public target::MLogit {
public:
  arma::vec weights;
  arma::uvec choice;

  MLogitR(const arma::uvec &alt,
	  const arma::uvec &id_idx,
	  const arma::mat &z1,
	  const arma::mat &z2,
	  const arma::mat &x,
	  const arma::uvec &choice,
	  const arma::vec &weights) :
    MLogit(alt, id_idx, z1, z2, x, 0) {
    this->weights = weights;	//
    this->choice = choice;
  }
  void hello() {
    Rcpp::Rcout << "Hello, world\n";
  }
  double loglik(const arma::vec &theta) {
    target::updateProb(theta);
    return as_scalar(sum(logpr%choice%weights));
  }
  rowvec score(const arma::vec &theta) {
    target::updateProb(theta);
    target::updateZX();

    vec pr = exp(logpr);
    mat xp = zx;
    xp.each_col() %= pr;
    xp = groupsum(xp, *cluster());
    uvec chosen  = find(choice);
    mat score = zx.rows(chosen) - xp;
    score.each_col() %= weights.elem(chosen);
    rowvec grad = sum(score,0); // Column-sums
    return grad;
  }

  mat hessian(const arma::vec &theta) {
    target::updateProb(theta);
    target::updateZX();

    vec pr = exp(logpr);
    mat xp = zx;
    xp.each_col() %= pr;
    mat tmp = xp;
    xp = zx - groupsum(xp, *cluster(), false);
    tmp.each_col() %= groupsum(weights%choice, *cluster(), false);
    mat hess = -xp.t()*tmp;
    return hess;
  }

  void update(const arma::vec &theta, unsigned basealt=0) {
    target::updateProb(theta);
    target::updateRef(basealt);
  }
};


RCPP_MODULE(MLogit){
    using namespace Rcpp ;

    class_<MLogitR>("MLogitR")
      .constructor<arma::uvec, // alt
		   arma::uvec, // id_idx
		   arma::mat,  // z1
		   arma::mat,  // z2
		   arma::mat,  // x
		   arma::uvec, // choice
		   arma::vec  // weights
		   >("Constructor")

      .method("logl",   &MLogitR::loglik,   "log-likelihood")
      .method("score",   &MLogitR::score,   "score")
      .method("hess",   &MLogitR::hessian,   "hessian")
      // .method("update", (void (MLogitR::*)(arma::vec, unsigned) )( &MLogitR::updateProb),
      // 	      "Update model parameters")
      .method("update", &MLogitR::update,
	      //List::create(_["theta"], _["ref"] = 0),
	      "Update model parameters")
      .method("hello", &MLogitR::hello, "Hello script")
      ;
}


// [[Rcpp::export(name=".mlogit_loglik")]]
double mlogit_loglik(arma::vec theta,
	       const arma::uvec &choice,
	       const arma::uvec &alt, unsigned basealt, unsigned nalt,
	       const arma::uvec &id_idx,
	       const arma::mat &z1, const arma::mat &z2, const arma::mat &x,
		     const arma::vec &weights) {
  target::MLogit dcm(alt, id_idx, z1, z2, x, nalt);
  dcm.updateRef(basealt);
  dcm.updateProb(theta);
  return (as_scalar(sum(dcm.logpr%choice%weights)));
}

// [[Rcpp::export(name=".mlogit_pred")]]
arma::mat mlogit_pred(arma::vec theta,
		     const arma::uvec &alt, unsigned basealt, unsigned nalt,
		     const arma::uvec &id_idx,
		     const arma::mat &z1, const arma::mat &z2, const arma::mat &x) {
  target::MLogit dcm(alt, id_idx, z1, z2, x, nalt);
  dcm.updateRef(basealt);
  dcm.updateProb(theta);
  vec pr = exp(dcm.logpr);
  return pr;
}


// Expand data from short to long form
// [[Rcpp::export(name=".mlogit_expand")]]
Rcpp::List mlogit_expand(const arma::uvec &alt,
			 const arma::mat &x,
			 const arma::vec &weights,
			 arma::uvec alts) {
  // uvec alts = unique(alt);
  unsigned J = alts.n_elem;
  unsigned ncl = alt.n_elem;
  unsigned n = J*ncl;
  uvec _choice(n); _choice.fill(0);
  uvec _alt(n);
  vec  _weights(n);
  uvec _id_idx(ncl);
  mat  _x(n, x.n_cols);
  unsigned pos = 0;
  for (unsigned i=0; i<ncl; i++) {
    _id_idx[i] = pos;
    for (unsigned j=0; j<J; j++) {
      _alt(pos) = alts(j);
      if (alts(j)==alt(i)) _choice(pos) = 1;
      _x.row(pos) = x.row(i);
      _weights(pos) = weights(i);
      pos++;
    }
  }
  return( List::create(Named("alt")=_alt,
		       Named("x")=_x,
		       Named("choice")=_choice,
		       Named("weights")=_weights,
		       Named("id_idx")=_id_idx) );
}

// mlogit_obj: returns the log-likelihood + score (and optionally hessian)
// [[Rcpp::export(name=".mlogit")]]
Rcpp::List mlogit_obj(arma::vec theta,
		      const arma::uvec &choice,
		      const arma::uvec &alt, unsigned basealt, unsigned nalt,
		      const arma::uvec &id_idx,
		      const arma::mat &z1, const arma::mat &z2, const arma::mat &x,
		      const arma::vec &weights,
		      bool return_hessian=false, bool onlyindiv=false) {

  target::MLogit dcm(choice, alt, id_idx, z1, z2, x, nalt, weights);

  dcm.updateRef(basealt);
  dcm.updateProb(theta);
  dcm.updateZX();

    // Log-likelihood
  vec loglik = dcm.logpr%choice%weights;
  vec pr = exp(dcm.logpr);

  // Score/gradient
  // vec r = (choice-pr); // raw residuals
  // mat score = dcm.zx;
  // score.each_col() %= r;
  mat xp = dcm.zx;
  xp.each_col() %= pr;
  xp = groupsum(xp, id_idx);
  uvec chosen  = find(choice);
  mat score = dcm.zx.rows(chosen) - xp;
  score.each_col() %= weights.elem(chosen);
  rowvec grad = sum(score,0); // Column-sums
  if (!return_hessian) {
    score = grad;
  }

  // Hessian
  mat hess(0,0);
  if (return_hessian) {
    xp = dcm.zx;
    xp.each_col() %= pr;
    mat tmp = xp;
    xp = dcm.zx-groupsum(xp, id_idx, false);
    tmp.each_col() %= groupsum(weights%choice, id_idx, false);
    hess = -xp.t()*tmp;
    // tmp = xp;
    // tmp.each_col() %= pr;
    // hess = -xp.t()*tmp;
  }

  return( List::create(Named("grad")=score,
		       Named("hess")=hess,
		       Named("ll")=sum(loglik)
		       ));
}


