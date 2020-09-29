// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <target/mlogit.hpp>
#include <target/utils.hpp>
#include <cmath>
#include <string>
#include <complex>
#include <vector>

using namespace Rcpp;
using namespace arma;
using namespace std;

class MLogitR : public target::MLogit {
public:
  MLogitR(uvec choice,
	  uvec alt,
	  uvec id_idx,
	  vector<mat> design_matrices,
	  vec weights
	  ) :
    MLogit(choice, alt, id_idx,
	   design_matrices[0], // z1
	   design_matrices[1], // z2
	   design_matrices[2], // x
	   0, weights) {}

  mat    pred(bool logarithm=false) {
    vec pr = this->logpr;
    if (logarithm) return pr;
    return exp(pr);
  }
  vec    par() { return MLogit::getPar(); }
  double loglik() { return MLogit::loglik(); }
  mat    hessian() { return MLogit::hessian(true); }
  mat    score(bool indiv=true) { return MLogit::score(true, indiv); }
  void   reference(unsigned basealt=0) { updateRef(basealt); }
  void   update(const vec &theta) { updateProb(theta); }
  List obj() {
    double l = MLogit::loglik();
    arma::mat s = MLogit::score(true, false);
    arma::mat h = MLogit::hessian(false);
    List res = List::create(Named("objective")=l,
			    Named("gradient")=s,
			    Named("hessian")=h);
    return res;
  }
  List info() {
    unsigned ref = this->basealt;
    List res = List::create(Named("ref")=ref,
			    Named("par")=MLogit::getPar());
    return res;
  }

};


RCPP_MODULE(dcmodel) {
    using namespace target ;

    class_<MLogitR>("dcmodel")
      .constructor<uvec,        // choice
		   uvec,        // alt
		   uvec,        // id_idx
		   vector<mat>, // z1,z2,x
		   vec          // weights
		   >("Constructor")
      .method("par",    &MLogitR::par,      "parameters")
      .method("obj",    &MLogitR::obj,      "log-likelihood, score, hessian")
      .method("logl",   &MLogitR::loglik,   "log-likelihood")
      .method("score",  &MLogitR::score,    "score")
      .method("hess",   &MLogitR::hessian,  "hessian")
      .method("update", &MLogitR::update,   "update model parameters")
      .method("ref",    &MLogitR::reference,"update reference alternative")
      .method("info",   &MLogitR::info,     "model information (reference, parameter indices)")
      ;
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


// [[Rcpp::export(name=".mlogit_pred")]]
arma::mat mlogit_pred(arma::vec theta,
		      const arma::uvec &alt, unsigned basealt, unsigned nalt,
		      const arma::uvec &id_idx,
		      const arma::mat &z1, const arma::mat &z2, const arma::mat &x,
		      bool logarithm=false) {

  target::MLogit dcm(arma::uvec(), alt, id_idx, z1, z2, x, nalt, arma::vec());  // choice, alternatives, id_idx, z1,z2, x, nalt, weights
  dcm.updateRef(basealt);
  dcm.updateProb(theta);
  if (logarithm) return dcm.logpr;
  vec pr = exp(dcm.logpr);
  return pr;
}
