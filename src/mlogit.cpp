#include "mlogit.hpp"

using namespace arma;

////////////////////////////////////////////////////////////////////////////////

namespace target {

  MLogit::MLogit(const arma::uvec &choice,
		 const arma::uvec &alt,
		 const arma::uvec &id_idx, 
		 const arma::mat &z1,
		 const arma::mat &z2,
		 const arma::mat &x,
		 unsigned nalt,
		 arma::vec weights) {
    n = alt.n_elem;
    J = nalt;
    ncl = id_idx.n_elem;
    if (weights.is_empty()) {
      weights = arma::vec(n);
      weights.fill(1);
    }
    updateData(choice, alt, id_idx, z1, z2, x, weights);
    
    if (nalt==0) {
      arma::uvec ualt = arma::unique(alt);
      J = ualt.n_elem;
    }
    idx_z1 = arma::uvec(z1.n_cols); // 
    unsigned pos=0;
    for (unsigned i=0; i<idx_z1.n_elem; i++) {
      idx_z1(i) = pos;
      pos++;
    }
    idx_z2 = arma::uvec(J*z2.n_cols);
    for (unsigned i=0; i<idx_z2.n_elem; i++) {
      idx_z2(i) = pos;
      pos++;
    }
    idx_x = arma::uvec((J-1)*x.n_cols);
    for (unsigned i=0; i<idx_x.n_elem; i++) {
      idx_x(i) = pos;
      pos++;
    }
    alt_idx = arma::uvec(J);
    updateRef(basealt);
    theta_z1 = arma::vec(z1.n_cols); 
    theta_x = arma::mat(J, x.n_cols);
    theta_z2 = arma::mat(J, z2.n_cols);
    theta = arma::vec(idx_z1.n_elem + idx_z2.n_elem + idx_x.n_elem);

    lp = arma::vec(n);
    logpr = arma::vec(n);
    zx = arma::sp_mat(n, theta.n_elem);	
  }  

  double MLogit::loglik() {
    return as_scalar(sum(logpr%(*Choice())%(*Weights())));
  }
  
  mat MLogit::score(bool update=false, bool indiv=true) {
    if (update) MLogit::updateZX();
    vec pr = exp(logpr);
    mat xp = zx;
    xp.each_col() %= pr;
    xp = target::groupsum(xp, *cluster(), true);
    uvec chosen  = find((*Choice()));
    mat score = zx.rows(chosen) - xp;
    score.each_col() %= (*Weights()).elem(chosen);
    if (!indiv) score = sum(score,0); // Column-sums
    return score;
  }
  
  mat MLogit::hessian(bool update=false) {
    if (update) MLogit::updateZX();
    vec pr = exp(logpr);
    mat xp = zx;
    xp.each_col() %= pr;
    mat tmp = xp;
    xp = zx - target::groupsum(xp, *cluster(), false);
    tmp.each_col() %= target::groupsum((*Weights())%(*Choice()), *cluster(), false);
    mat hess = -xp.t()*tmp;
    return hess;
  }
  
  void MLogit::updateRef(unsigned basealt) {
    // Sets the base/reference alternative
    this->basealt = basealt;
    alt_idx.fill(0);
    unsigned pos=1;
    for (unsigned j=0; j<J; j++) {
      if (j!=basealt) {
	this->alt_idx[j] = pos;
	pos++;
      }
    }
  }
  
  void MLogit::updatePar(vec theta) {
    this->theta = theta;
    // theta = (z1_1,...,z1_n1, z2_1, ..., z2_J*n2, x1_1, ..., x_1_(J-1)*n3)
    for (unsigned i=0; i<p_z1; i++) {
      theta_z1(i) = theta(idx_z1[i]);
    }
    theta_x.fill(0);
    theta_z2.fill(0);
    unsigned pos = 0;  
    if (p_x>0) {
      for (unsigned j=0; j<J; j++)
	if (j!=basealt)
	  for (unsigned i=0; i<p_x; i++) {      
	    theta_x(j,i) = theta(idx_x[pos]);
	    pos++;
	  }
    }
    pos = 0;
    if (p_z2>0) {
      for (unsigned j=0; j<J; j++)
	for (unsigned i=0; i<p_z2; i++) {	
	  theta_z2(j,i) = theta(idx_z2[pos]);
	  pos++;
	}
    }
  }


  void MLogit::updateZX() {
    unsigned pos;
    for (unsigned i=0; i<n; i++) {
      pos = 0;      
      if (p_z1>0) {
	for (unsigned j=0; j<p_z1; j++)
	  zx(i,j) = (*Z1())(i,j);
	pos = p_z1;
      }
      if (p_z2>0) {
	for (unsigned j=0; j<p_z2; j++)
	  zx(i, j + pos + p_z2*Alt(i)) = (*Z2())(i,j);
	pos += p_z2*J;
      }
      if (p_x>0) {
	if (Alt(i)!=basealt) {
	  pos += p_x*(alt_idx(Alt(i))-1);
	  for (unsigned j=0; j<p_x; j++)
	    zx(i,j+pos) = (*X())(i,j);
	}
      }
    }
  }
  
  void MLogit::updateProb() {
    lp.fill(0); // Linear predictor
    if (p_z1>0) {
      lp = (*Z1())*theta_z1;
    }
    if (p_z2>0)
      for (unsigned i=0; i<n; i++) {
	  rowvec z2 = Z2(i);
	  lp[i] += as_scalar((*Z2()).row(i)*trans(theta_z2.row(Alt(i))));
      }
    if (p_x>0) 
      for (unsigned i=0; i<n; i++) {
    	if (Alt(i)!=basealt) {
    	  lp[i] += as_scalar((*X()).row(i)*trans(theta_x.row(Alt(i))));
    	}
      }    
    
    // Calculate log-probabilities of choices
    unsigned start, stop;    
    unsigned pos = 0;
    for (unsigned i=0; i<ncl; i++) { // Iterate over individuals 
    	start = this->cluster(i);
    	if (i==(ncl-1)) {
    	  stop = n-1;
    	} else {
    	  stop = this->cluster(i+1)-1;
    	}
    	vec a = target::softmax( lp.subvec(start, stop) ); // Multinomial logit / softmax
    	for (unsigned j=0; j<a.n_elem; j++) {	
    	  logpr(pos) = a(j);
    	  pos++;
    	}
    }    
  }
  
} // namespace target

////////////////////////////////////////////////////////////////////////////////


