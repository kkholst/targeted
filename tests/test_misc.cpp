/*!
  @file test_misc.cpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Unit tests

*/

#include <catch2/catch.hpp>
#include <spdlog/spdlog.h>
#include "utils.hpp"

using namespace arma;

TEST_CASE("Armadillo check", "[arma]") {
  vec x = {1,2,3};
  spdlog::info("Size: {:d}", x.n_elem);
  std::cout << trans(x) << std::endl;
  REQUIRE(x.n_elem == 3);
}

TEST_CASE("Cluster-id", "[utils]") {
  
  SECTION("Test clusterid") {    
    uvec inp = {1,1,2,2,2};
    umat res = target::clusterid(inp);
    REQUIRE(res.n_rows == 2);
    REQUIRE(res(0,1) == 2); // size
    REQUIRE(res(1,1) == 3);
    REQUIRE(res(0,0) == 0); // index of first element in cluster
    REQUIRE(res(1,0) == 2);
  }

}

////////////////////////////////////////////////////////////////////////////////

bool True() { return(true); }

TEST_CASE("Sanity check", "[sanity]") {
  spdlog::info("Sanity checks!");
  REQUIRE(True());
  REQUIRE(2 == 2);
  CHECK(1 == 1); // Continue even if test fails
  REQUIRE(100.5 == Approx(100).epsilon(0.01)); // Allow 1% difference
  REQUIRE(100 == Approx(100));
}



