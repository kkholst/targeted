#pragma once

#include "glm.hpp"
#include "utils.hpp"

namespace target {
  
class MLogit
{
private:

  arma::mat _z1; // alternative-specific with constant coef.
  arma::mat _z2; // alternative-specific with varying coef.
  arma::mat _x;  // individual specific covariates  
  arma::uvec _id_idx; // Index of start of each cluster (choice observations for each individual is a 'cluster')
  arma::uvec _alt;    // Vector of alternatives
  arma::uvec _choice; // alternative-specific with constant coef.  
  // Vector of unique alternatives (base-ref: 0)
  arma::uvec alt_idx;
  arma::vec _weights;

  // Parameters
  arma::vec theta_z1;
  arma::mat theta_z2;
  arma::mat theta_x;
  arma::vec theta;
  arma::uvec idx_z1; // Index of the parameters belonging to 'z1'
  arma::uvec idx_z2; // Index of the parameters belonging to 'z2'
  arma::uvec idx_x;  // Index of the parameters belonging to 'x'
  
 
public:
  unsigned n;           // Number of observations in long format (individuals and choices)
  unsigned ncl;         // Number of individuals
  unsigned J;           // Number of alternatives
  unsigned basealt = 0; // Base alternative  
  arma::vec lp;
  arma::mat zx;
  arma::vec logpr;
  unsigned p_z1; // Number of columns in z1
  unsigned p_z2; // Number of columns in z2
  unsigned p_x;  // Number of columns in x
  
  MLogit() {}; // Empty constructor
  MLogit(const arma::uvec &choice,
	 const arma::uvec &alt,
	 const arma::uvec &id_idx, 
	 const arma::mat &z1,
	 const arma::mat &z2,
	 const arma::mat &x,
	 unsigned nalt=0,
	 arma::vec weights=arma::vec());
  
  arma::mat hessian(bool update);
  arma::mat score(bool update, bool indiv);
  double loglik();
  void updateZX();
  void updateRef(unsigned basealt);
  void updatePar(arma::vec theta);
  void updateProb();
  void updateProb(arma::vec theta) {
    updatePar(theta);
    updateProb();
  }

  const arma::vec*  Weights() { return &_weights; }
  double            Weights(unsigned i) { return _weights(i); }

  const arma::uvec* Choice() { return &_choice; }
  unsigned          Choice(unsigned i) { return _choice(i); }

  const arma::uvec* Alt() { return &_alt; }
  unsigned          Alt(unsigned i) { return _alt(i); }
  
  const arma::mat* X() { return &_x; }
  arma::rowvec     X(unsigned row) { return _x.row(row); }
  
  const arma::mat* Z1() { return &_z1; }
  arma::rowvec     Z1(unsigned row) { return _z1.row(row); }
  
  const arma::mat* Z2() { return &_z2; }
  arma::rowvec     Z2(unsigned row) { return _z2.row(row); }
  
  const arma::uvec* cluster() { return &_id_idx; }
  unsigned          cluster(unsigned index) { return _id_idx(index); }

  void updateData(const arma::uvec &choice,
		  const arma::uvec &alt,
		  const arma::uvec &id_idx,
		  const arma::mat &z1,
		  const arma::mat &z2,
		  const arma::mat &x,
		  const arma::vec &weights) {
    _choice = choice;
    _z1 = z1;
    _z2 = z2;
    _x = x;
    _id_idx = id_idx;
    _alt = alt;
    p_z1 = z1.n_cols;
    p_z2 = z2.n_cols;
    p_x = x.n_cols;    
  }

};

}  // namespace target

