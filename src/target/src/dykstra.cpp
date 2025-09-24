/*!
  @file dykstra.cpp
  @author Klaus Kähler Holst
  @copyright 2025, Klaus Kähler Holst

  @brief Dykstra's alternating projection algorithm for
  least squares problems with inequality constraints
*/
#include <target/dykstra.hpp>


//  projection  min ||u-|^2 s.t. one-dimensional constraint Aj x <= 0
arma::vec proj1(arma::vec u, arma::vec Aj, arma::vec w) {
  double b = sum(Aj % u);
  if (b <= 0)
    return (u);
  return u - Aj * (b / sum(Aj % Aj));
  // w = 1 / w;
  // return u - w % Aj * b / sum(Aj % Aj % w);
}

namespace target {

  SignedWald signedwald_sim(const arma::vec &par, const arma::mat &vcov,
                            const arma::vec &noninf, const arma::vec &weights,
                            unsigned nsim_null) {
    unsigned p = par.n_elem;
    arma::vec e;
    arma::mat P;
    arma::eig_sym(e, P, vcov);
    e = arma::sqrt(e);
    arma::mat D = arma::diagmat(e);
    arma::mat iD = arma::diagmat(1 / e);
    arma::mat sqrt_sigma = P * D * P.t();
    arma::mat sqrt_sigma_inv = P * iD * P.t();
    arma::vec uhat = sqrt_sigma_inv * (weights % (par - noninf));
    // ||uhat-u||^2 = (uhat-u)^T(uhat -h) s.t. sqrt_sigma * u <= 0
    raggedArray opt = lsdykstra(uhat, sqrt_sigma);
    arma::vec u0 = opt[0];
    double sw = sum(arma::square(uhat-u0));
    arma::vec sw_sim(nsim_null);
    for (unsigned i = 0; i < nsim_null; i++) {
      arma::vec usim = arma::randn(p) % weights;
      raggedArray opt = lsdykstra(usim, sqrt_sigma);
      sw_sim[i] = sum(arma::square(usim - opt[0]));
    }
    double pval = sum(sw_sim >= sw) / (double)nsim_null;
    SignedWald res;
    res.sw = sw;
    res.pval = pval;
    res.sol = u0;
    return res;
  }

  raggedArray lsdykstra(const arma::vec &x, const arma::mat &A,
                        double tol, unsigned iter_max) {
    unsigned n = x.n_elem;
    unsigned J = A.n_rows; // number of constraints
    arma::vec xn = x;
    arma::vec w = arma::ones(n);
    double err = 1;
    unsigned iter = 1;
    arma::mat y = arma::zeros(n, n);
    // std::cout << "hej\n";
    while (err > tol && iter < iter_max) {
      iter++;
      arma::vec xprev = xn;
      for (unsigned j = 0; j < J; j++) {
        arma::vec d = xn - y.col(j);
        xn = proj1(d, A.row(j).t(), w);
        y.col(j) = xn - d;
      }
      err = arma::norm(xn - xprev);
    }
    arma::vec niter(1);
    niter[0] = iter;
    raggedArray res(2);
    res[0] = xn;
    res[1] = niter;
    return res;
  }


}
