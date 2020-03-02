#include <target/target.h>
#include <armadillo>


int main(int argc, char **argv) {

  arma::vec y, w, p;
  arma::mat a,x1,x2;
  std::cout << "* Simulating data...\n\n";
  arma::arma_rng::set_seed(1);
  unsigned n=5;
  y = arma::vec(n, 1);   y.randn();
  w = arma::mat(n, 1);  w.fill(1);
  a = arma::randi<arma::mat>(n, 1, arma::distr_param(0, 1));
  x2 = arma::mat(n, 2); x2.randn();
  x1 = arma::mat(n, 1); x1.fill(1);
  p = arma::vec(4); p.fill(0.5);

  n = std::min(3, (int)y.n_elem);

  target::RD<double> model(y, a, x1, x2, x2, p, w);
  arma::vec res = model.loglik();
  std::cout << "res=" << res << std::endl;
}

  
