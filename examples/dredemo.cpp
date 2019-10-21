/*!
  @file main.cpp
  @author Klaus K. Holst
  @copyright 2018, Klaus Kähler Holst

  @brief Example file

*/

//#define ARMA_USE_HDF5
#define MATHLIB_STANDALONE
#include <armadillo>
#include <cstdio>  // remove
#include <algorithm> // max
#include "target.hpp"
#include "utils.hpp"

using namespace arma;

int main(int argc, char **argv) {

  vec y, w, p;
  mat a,x1,x2;
  if ( argc>1 ) {
    const char *infile = argv[1];
    std::cout << "* Reading from '" << infile << "'\n\n";
  } else {
    std::cout << "* Simulating data...\n\n";
    arma_rng::set_seed(1);
    unsigned n=5;
    y = vec(n, 1);   y.randn();
    w = mat(n, 1);  w.fill(1);
    a = randi<mat>(n, 1, distr_param(0, 1));
    x2 = mat(n, 2); x2.randn();
    x1 = mat(n, 1); x1.fill(1);
    p = vec(4); p.fill(0.5);
  }

  unsigned n = std::min(3, (int)y.n_elem);
  std::cout << " Response:\n" << BLUE << y.rows(0,n-1) << "\t..." << COL_RESET << std::endl;
  std::cout << " Exposure:\n" << CYAN << a.rows(0,n-1) << "\t..." << COL_RESET << std::endl;
  std::cout << " X1:\n" << YELLOW << x1.rows(0,n-1) << "\t..." << COL_RESET << std::endl;
  std::cout << " X2:\n" << YELLOW << x2.rows(0,n-1) << "\t..." << COL_RESET << std::endl;
  std::cout << "\n parameter: " << GREEN << p.t() << COL_RESET<< std::endl;

  target::RD<double> model(y, a, x1, x2, x2, p, w);
  vec res = model.loglik();
  std::cout << " loglik=\n" << RED << res << std::endl << COL_RESET;

  mat U = model.score(true);
  std::cout << " score=\n" << RED << U << std::endl << COL_RESET;

  arma::vec alpha2(1); alpha2.fill(1);
  U = model.est(alpha2);
  std::cout << " U=\n" << RED << U << std::endl << COL_RESET;

  arma::mat pp = model.TargetBinary<double>::pa();
  std::cout << "pp=\n" << pp << std::endl;


  // const char *filen = "tmp/a.h5";
  // std::remove(filen);
  // y.save(hdf5_name(filen, "y", hdf5_opts::append+hdf5_opts::trans));
  // a.save(hdf5_name(filen, "a", hdf5_opts::append+hdf5_opts::trans));
  // w.save(hdf5_name(filen, "w", hdf5_opts::append+hdf5_opts::trans));
  // x1.save(hdf5_name(filen, "x1", hdf5_opts::append+hdf5_opts::trans));
  // x2.save(hdf5_name(filen, "x2", hdf5_opts::append+hdf5_opts::trans));
  // p.save(hdf5_name(filen, "p", hdf5_opts::append+hdf5_opts::trans));

  return 0;
}



