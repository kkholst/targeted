/*!
  @file pava.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Pooled Adjacent Violator Algorithms

*/
#include <cmath>
#include <target/utils.hpp>


namespace target {

  /*!
    Pooled Adjacent Violator Algorithm for

    @param y Response variable
    @param x Optional covariate to order the response variable after
    @param w Optional weight vector
  */
  arma::mat pava(arma::vec y,
		 const arma::vec &x,
		 arma::vec w) {
    unsigned n = y.n_elem;
    unsigned nb = n;  // Number of blocks
    if (!x.is_empty()) {
      if (x.n_elem != n)
	throw std::range_error("Wrong length of predictor variable 'x'");
    }
    if (w.is_empty()) {
      w.resize(n);
      for (unsigned i=0; i < n; i++) w[i]=1;
    } else {
      if (w.n_elem != n)
	throw std::range_error("Wrong length of weights variable 'weights'");
    }
    std::vector<unsigned> poolEnd(n);
    // Initialize with n pools (each observation defines a block)
    for (unsigned i=0; i < n; i++) poolEnd[i] = i;

    unsigned i1, i2;
    double w0;
    while (true) {
      bool stable = true;
      unsigned i = 0;
      unsigned nviolators = 0;
      while (i < (nb-1)) {
	unsigned pos = i+nviolators;
	poolEnd[i] = poolEnd[pos];
	poolEnd[i+1] = poolEnd[pos+1];
	i1 = poolEnd[i];
	i2 = poolEnd[i+1];
	if (y[i1] >= y[i2]) {  // Violator => pool new observation with current block
	  stable    = false;
	  w0 = w[i1]+w[i2];
	  y[i2]     = (w[i1]*y[i1]+w[i2]*y[i2])/w0;
	  w[i2]     = w0;
	  poolEnd[i] = poolEnd[i+1];
	  nviolators++;
	  nb--;
	}
	i++;
      }
      poolEnd[nb-1] = n-1;
      if (stable) break;
    }

    arma::mat res(nb, 2);
    for (unsigned i=0; i < nb; i++) {
      res(i, 0) = y[poolEnd[i]];
    }
    res(0, 1) = 1;
    for (unsigned i=0; i < (nb-1); i++) {
      res(i+1, 1) = poolEnd[i]+1;  // Beginning of each pool right after
                                   // previous pool ends
    }
    return res;
  }

}  // namespace target
