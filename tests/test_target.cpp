
/*!
  @file test_mlogit.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Unit tests for discrete choice models components

*/

#include <catch2/catch.hpp>
#include <spdlog/spdlog.h>
#include "target.hpp"
#include "utils.hpp"

using namespace arma;
using namespace target;

TEST_CASE("likelihood", "[target]") {
  
  SECTION("loglik1") {
    spdlog::info("Testing likelihood!");
    arma_rng::set_seed(1);
    unsigned n = 5;
    colvec y(n, 1);   y.randn();
    colvec w(n, 1);  w.fill(1);
    mat a = arma::randi<mat>(n, 1, arma::distr_param(0, 1));
    mat x2(n, 2); x2.randn();
    mat x1(n, 1); x1.fill(1);

    colvec p(4); p.fill(0.5);
    target::RD <double> model(y, a, x1, x2, x2, p, w);
    vec res = model.loglik();

    mat pp = model.pa();
    double logl = sum(y%log(pp.col(0)) + (1-y)%log(1-pp.col(0)));
    std::cout << RED << res[0] << std::endl << COL_RESET;
    std::cout << BLUE << logl << std::endl << COL_RESET;
    REQUIRE(logl == res[0]);

    vec w2 = w*2;
    model = target::RD<double>(y, a, x1, x2, x2, p, w2);
    REQUIRE(2*logl == model.loglik()[0]);    
  }

}


  // TEST(Score) {
  //   arma_rng::set_seed(1);
  //   unsigned n = 5;
  //   colvec y(n, 1);   y.randn();
  //   colvec w(n, 1);  w.fill(1);
  //   mat a = arma::randi<mat>(n, 1, arma::distr_param(0, 1));
  //   mat x1(n, 2); x1.randn();
  //   mat x2 = x1, x3 = x1;

  //   colvec p(6); p.fill(0.5);
  //   colvec alpha(2); p.fill(1);
  //   target::RR <double> model(y, a, x1, x2, x2, p, w);

  //   mat pp0 = model.TargetBinary::pa();
  //   mat U = model.score(false);
  //   mat res = model.est(alpha);

  //   colvec gamma = {p[4], p[5]};
  //   vec pr = x2*gamma;
  //   mat res2 = model.est(alpha, pr);
  //   std::cout << std::endl << RED << res[0] << std::endl << COL_RESET;
  //   std::cout << std::endl << RED << res2[0] << std::endl << COL_RESET;

  //   CHECK_CLOSE(2, 2, 1e-9);
  // }
