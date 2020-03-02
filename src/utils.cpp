
/*!
  @file utils.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Various utility functions and constants

*/
#include "utils.hpp"

namespace target {
  
  // Complex step derivative
  arma::mat deriv(cx_func f, arma::vec theta) {
    arma::cx_vec thetac = arma::conv_to<arma::cx_vec>::from(theta);
    arma::cx_mat val0 = f(thetac);
    unsigned n = val0.n_elem;
    unsigned p = theta.n_elem;
    arma::mat res(n,p);
    double h = DBL_MIN;
    cx_dbl h0 = cx_dbl(0, h);
    for (unsigned i=0; i<p; i++) {
      arma::cx_vec theta0 = thetac;
      theta0[i] += h0;
      arma::mat val = imag(f(theta0))/h;
      for (unsigned j=0; j<n; j++)
	res(j,i) = val[j];      
    }
    return(res);
  }  


  // Find index of clusters/ids in sorted integer vector
  arma::umat clusterid(const arma::uvec &id) {
    unsigned ncl = 1;
    unsigned n = id.size();
    unsigned id0 = id(0);
    for (unsigned i=0; i<n; i++) {
      if (id(i)!=id0) {
	ncl++;
	id0 = id(i);
      }
    }
    arma::umat res(ncl,2); // column 1: index, column 2: size
    res.fill(0);
    unsigned cl = 0;
    id0 = id(0);
    for (unsigned i=0; i<n; i++) {
      if (id(i)!=id0) {
	cl++;
	res(cl,0) = i; // index
	id0 = id(i);
      }
      res(cl,1)++; // size
    }
    return res;
  }

  
  arma::mat groupsum(const arma::mat &x,
		     const arma::uvec &cluster,
		     bool reduce=true) {
    unsigned ncl = cluster.n_elem;
    unsigned n = ncl;
    if (!reduce) {
      n = x.n_rows;
    }
    unsigned start, stop;
    arma::mat res(n, x.n_cols);
    arma::rowvec tmp(x.n_cols);
    for (unsigned i=0; i<ncl; i++) { // Iterate over individuals 
      start = cluster(i);
      if (i==(ncl-1)) {
	stop = x.n_rows;
      } else {
	stop = cluster(i+1);
      }
      tmp.fill(0);      
      for (unsigned j=start; j<stop; j++) {
	tmp += x.row(j);
      }
      if (reduce) {
	res.row(i) = tmp;
      } else {
	for (unsigned j=start; j<stop; j++) {
	  res.row(j) = tmp;
	}
      }
    }
    return(res);
  }


  double SupTest(const arma::vec &D) {
    return arma::max(arma::abs(D));
  };

  double L2Test(const arma::vec &D, const arma::vec &t) {
    arma::vec delta(t.n_elem);
    for (unsigned i=0; i<t.n_elem-1; i++) delta(i) = t[i+1]-t[i];
    delta(delta.n_elem-1) = 0;
    return std::sqrt(sum(delta % D % D));
  }

  // Comparison of ECDF of (x1,...,xn) with null CDF evaluated in G = (G(x1),...,G(xn))
  double CramerVonMises(const arma::vec &x, arma::vec G) {
    arma::uvec ord = arma::stable_sort_index(x); // back to original order of input data    
    G = G.elem(ord);
    unsigned n = G.n_elem;
    double res = 1/(12*n);
    for (unsigned i=0; i<G.n_elem; i++) {
      double val = (2*i-1)/(2*n)-G(i);
	res += val*val;
    }
    return res;
  }
 
  
  arma::mat const EmptyMat = arma::mat();
  arma::vec const EmptyVec = arma::vec();  

  
  // Foreground colors are in form of 3x, bacground are 4x
  const char* COL_RESET = "\x1b[0m";
  const char* COL_DEF   = "\x1b[39m";
  const char* BLACK     = "\x1b[30m";
  const char* RED       = "\x1b[31m";
  const char* MAGENTA   = "\x1b[35m";
  const char* YELLOW    = "\x1b[33m";
  const char* GREEN     = "\x1b[32m";
  const char* BLUE      = "\x1b[34m";
  const char* CYAN      = "\x1b[36m";
  const char* WHITE     = "\x1b[37m";
  const char* GRAY      = "\x1b[90m";
  const char* LRED      = "\x1b[91m";
  const char* LGREEN    = "\x1b[92m";
  const char* LYELLOW   = "\x1b[93m";
  const char* LBLUE     = "\x1b[94m";
  const char* LMAGENTA  = "\x1b[95m";
  const char* LCYAN     = "\x1b[96m";
  const char* LWHITE    = "\x1b[97m";

} // namespace target
