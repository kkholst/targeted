/*!
  @file main.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Example file

*/

//#define ARMA_USE_HDF5
#include "cumres.hpp"
#include "glm.hpp"
#include "pava.hpp"
#include <cstdio>  // remove
#include <algorithm> // max

using namespace arma;
using namespace target;


int main(int argc, char **argv) {
  cout << BLUE << "Armadillo version: "
       << arma_version::as_string() << COL_RESET << endl;
  
  std::string infile = "misc/surgunit.csv";
  if ( argc>1 ) {
    infile = argv[1];
    std::cout << "* Reading from '" << infile << "'\n\n";
  }
 
  mat d;
  field<std::string> header(0);
  d.load( arma::csv_name(infile, header) );
  vec y = d.col(0);
  mat x = d.cols(1,2);  
  std::cout << "Pava: \n";
  arma::mat pres = pava(y);
  std::cout << pres << std::endl;
 

  unsigned n = y.n_elem;
  vec ones(n); ones.fill(1);
  mat X = join_horiz(ones, x);
  mat iX2 = inv(X.t()*X);
  mat hat = iX2*X.t();

  unsigned N = std::min(10, (int)n);
  std::cout << " Response:\n" << BLUE << y.rows(0,N-1) << "\t..." << COL_RESET << std::endl;
  std::cout << " X:\n" << YELLOW << X.rows(0,N-1) << "\t..." << COL_RESET << std::endl;

  mat beta = hat*y;
  vec yhat = X*beta;
  vec r = y-yhat;
  mat ic = linear_iid(y, yhat, x, ones).iid;

  vec t = X.col(1);
  uvec ord = stable_sort_index(t);

  std::cout << " beta:\n" << RED << beta.t() << COL_RESET;

  std::cout << " \nr:\t" << r.rows(0,N-1).t() << COL_RESET;

  arma_rng::set_seed_random();
  target::cumres proc(r, -x, ic);
  proc.order(t);
    
  vec res = proc.sample();
  //std::cout << GREEN << "... " << trans(res.subvec(N-10,N))  << std::endl << std::endl;

  vec obs = proc.obs();
  double ks0 = target::SupTest(obs);
  mat stats = proc.sample(10000);

  std::cout << stats.rows(0,N-1) << std::endl;
  
  vec ks = stats.col(0);
  std::cout << "KS:\t" << YELLOW << trans(ks.subvec(0,N-1));
  uvec idx = arma::find(ks>ks0);
  double pval = idx.n_elem/(double)ks.n_elem;
  std::cout << std::endl <<
    "p-value = " << BLUE << pval << COL_RESET << std::endl;
  
  // arma::vec ks = proc.sample(100).col(0);
  // std::cout << RED << trans(ks.subvec(0,10)) << std::endl << std::endl;
  // vec obs = proc.obs();
  // double ks0 = target::SupTest(obs);
  // uvec idx = arma::find(ks>ks0);
  // double pval = idx.n_elem/(double)ks.n_elem;
  // std::cout << BLUE << "p-value = " << pval << std::endl;

  // unsigned N = r.n_elem;
  // idx = arma::uvec(10);
  // for (unsigned i=0; i<idx.n_elem-1; i++) {
  //   idx(i) = i*(N/10);
  // }
  // idx(idx.n_elem-1) = N-1;
  
  // vec what = proc.sample(idx);
  // std::cout << RED << trans(what) << std::endl << std::endl;

  
  return 0;
}



