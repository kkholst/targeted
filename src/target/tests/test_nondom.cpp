/*!
  @file test_nondom.cpp
  @author Klaus Kähler Holst
  @copyright 2023, Klaus Kähler Holst

  @brief Unit tests for non-dominated sets algorithm

*/
#include <doctest/doctest.h>
#include <spdlog/spdlog.h>
#include <target/target.hpp>
#include <target/nondom.hpp>

using namespace arma;
using namespace target;

TEST_CASE("likelihood") {

    spdlog::info("Testing nondom:");

    arma::mat x = {
      {1.0, 0.5},
      {0.0, 1.0},
      {1.0, 0.0},
      {0.5, 1.0},
      {1.0, 1.0},
      {0.8, 0.8}
  };

    arma::mat res = target::nondominated(x);
    std::cout << RED << res << std::endl << COL_RESET;

    REQUIRE(res.n_rows == 3);
}
