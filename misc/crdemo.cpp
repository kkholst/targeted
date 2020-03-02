/*!
  @file main.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Example file

*/

//#define ARMA_USE_HDF5
#include "cumres.hpp"
#include "pava.hpp"
#include <cstdio>  // remove
#include <algorithm> // max

using namespace arma;
using namespace target;


int main(int argc, char **argv) {
  std::cout << "Pava: \n";
  return 0;
  
  std::string infile = "misc/d.csv";
  std::string infile2 = "misc/r.csv";  
  if ( argc>1 ) {
    infile = argv[1];
    std::cout << "* Reading from '" << infile << "'\n\n";
  }
  if ( argc>2 ) {
    infile = argv[2];
    std::cout << "* Reading from '" << infile << "'\n\n";
  }  
 
  mat d;
  d.load(infile, arma::csv_ascii);
  vec y = d.col(0);
  mat x = d.cols(1,2);
  x.insert_cols(0,1); x.col(0) += 1;

  std::cout << "Pava: \n";
  arma::mat pres = pava(y);
  std::cout << pres << std::endl;
  return 0;
  
  // else {
  // // std::cout << "* Simulating data...\n\n";
  // arma_rng::set_seed(1);
  // unsigned n=5;
  // y = vec(n, 1);   y.randn();
  // w = mat(n, 1);  w.fill(1);
  // a = randi<mat>(n, 1, distr_param(0, 1));
  // x2 = mat(n, 2); x2.randn();
  // x1 = mat(n, 1); x1.fill(1);
  // p = vec(4); p.fill(0.5);

  unsigned n = std::min(10, (int)y.n_elem);
  std::cout << " Response:\n" << BLUE << y.rows(0,n-1) << "\t..." << COL_RESET << std::endl;
  std::cout << " X:\n" << YELLOW << x.rows(0,n-1) << "\t..." << COL_RESET << std::endl;
  // std::cout << "\n parameter: " << GREEN << p.t() << COL_RESET<< std::endl;

  mat d2;
  d2.load(infile2, arma::csv_ascii);
  vec r = d2.col(0);
  vec t = d2.col(1);
  uvec ord = stable_sort_index(t); 
  mat ii = d2.cols(2,d2.n_cols-1);
    
  std::cout << " r:\n" << BLUE << r.rows(0,n-1).t() << "\t..." << COL_RESET << std::endl;
  std::cout << " t:\n" << YELLOW << t.rows(0,n-1).t() << "\t..." << COL_RESET << std::endl;


  arma_rng::set_seed_random();
  target::cumres proc(r, -x, ii);
  proc.reorder(arma::conv_to<arma::vec>::from(ord));
  vec res = proc.sample();
  std::cout << GREEN << "... " << trans(res.subvec(n-10,n))  << std::endl << std::endl;
  
  arma::vec ks = proc.sample(100).col(0);
  std::cout << RED << trans(ks.subvec(0,10)) << std::endl << std::endl;
  vec obs = proc.obs();
  double ks0 = target::SupTest(obs);
  uvec idx = arma::find(ks>ks0);
  double pval = idx.n_elem/(double)ks.n_elem;
  std::cout << BLUE << "p-value = " << pval << std::endl;

  unsigned N = r.n_elem;
  idx = arma::uvec(10);
  for (unsigned i=0; i<idx.n_elem-1; i++) {
    idx(i) = i*(N/10);
  }
  idx(idx.n_elem-1) = N-1;
  
  vec what = proc.sample(idx);
  std::cout << RED << trans(what) << std::endl << std::endl;

  ks = proc.sample(1000, idx).col(0);
  std::cout << RED << trans(ks.subvec(0,10)) << std::endl << std::endl;
  idx = arma::find(ks>ks0);
  pval = idx.n_elem/(double)ks.n_elem;
  std::cout << BLUE << "p-value = " << pval << std::endl;
  
  return 0;
}



