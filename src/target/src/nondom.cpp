/*!
  @file nondom.cpp
  @author Klaus Kähler Holst
  @copyright 2023-2025, Klaus Kähler Holst

  @brief Non-dominated sets (Kung et al. 1975)

*/
#include <target/nondom.hpp>
#include <cassert>
#include <cmath>


namespace target {

  bool nondom(const arma::rowvec &x, const arma::mat &y) {
    assert(x.n_elem == y.n_cols);
    // check if vector x is non-dominated by all rows in y
    for (unsigned i=0; i < y.n_rows; i++) {
      bool dom = true;
      for (unsigned k=0; k < x.n_elem; k++) {
        dom = dom && (y(i, k) <= x(k));
        if (!dom) break;
      }
      if (dom) return false;
    }
    return true;
  }

  arma::mat Front(const arma::mat &x) {
    unsigned n = x.n_rows;
    if (n==1) return x;
    unsigned n1 = std::floor(x.n_rows / 2);
    // Split objective values into top and bottom part
    // Call Front recursively
    arma::mat top = Front(x(arma::span(0,n1-1), arma::span::all));
    arma::mat bottom = Front(x(arma::span(n1, x.n_rows-1), arma::span::all));
    for (unsigned i = 0; i < bottom.n_rows; i++) {
      // If elements in bottom part is non-dominated by top elemnts, then append the vector
      if (nondom(bottom.row(i), top)) {
        top = arma::join_cols(top, bottom.row(i));
      }
    }
    return top;
  }

  /*!
    Non-dominated set (Kung et al. 1975)

    @param x matrix with objective values (nxp matrix with p the dimension of the objective)

  */
  arma::mat nondominated(const arma::mat &x) {
    // Sort rows according to first column of objective values
    arma::uvec indices = arma::stable_sort_index(x.col(0), "ascend");
    // Recursive call Front function
    arma::mat res = Front(x.rows(indices));
    // As the initial sorting might contain ties, we need to run through
    // the identified non-dominated candidate set and remove any possible
    // dominated terms
    if (res.n_rows==1) return(res);
    arma::uvec pos(res.n_rows); // indicator of actual non-dominated elements
    pos.fill(0);
    for (unsigned i=0; i < res.n_rows; i++) {
      arma::rowvec r = res.row(i);
      arma::vec ci = res.col(0);
      ci.shed_row(i);
      arma::uvec ties = arma::find(ci == r(0));
      if (ties.n_elem > 0) {
        if (!nondom(r, res.rows(ties))) {
          pos(i) = 1;
        }
      } else {
        pos(i) = 1;
      }
    }
    return res.rows(find(pos==1));
  }

}  // namespace target
