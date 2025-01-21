/*!
  @file test_misc.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Unit tests

*/

#include <doctest/doctest.h>
#include <spdlog/spdlog.h>
#include <target/utils.hpp>

using namespace arma;


TEST_CASE("Armadillo check") {
  vec x = {1,2,3};
  spdlog::info("Size: {:d}", x.n_elem);
  std::cout << trans(x) << std::endl;
  REQUIRE(x.n_elem == 3);
}

TEST_CASE("Utilities: Test clusterid") {
    uvec inp = {1,1,2,2,2};
    umat res = target::clusterid(inp);
    REQUIRE(res.n_rows == 2);
    REQUIRE(res(0,1) == 2); // size
    REQUIRE(res(1,1) == 3);
    REQUIRE(res(0,0) == 0); // index of first element in cluster
    REQUIRE(res(1,0) == 2);
}

TEST_CASE("Utilities: Fastapprox") {
    vec time = {1.0, 2.0, 3.0, 4.0};
    vec newtime = {3.1, 0.1};
    umat res = target::fastapprox(time, newtime, false, 0);
    REQUIRE(res.n_rows == 2);
    REQUIRE(res.n_cols == 1);
    REQUIRE(res(0) == 2);
    REQUIRE(res(1) == 0);
}

////////////////////////////////////////////////////////////////////////////////

bool True() { return(true); }

TEST_CASE("Sanity check") {
  spdlog::info("Sanity checks!");
  REQUIRE(True());
  REQUIRE(2 == 2);
  CHECK(1 == 1); // Continue even if test fails
  REQUIRE(100.5 == doctest::Approx(100).epsilon(0.01)); // Allow 1% difference
  REQUIRE(100 == doctest::Approx(100));
}

