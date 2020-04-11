/*!
  @file test_c.cpp
  @author Klaus K. Holst
  @copyright 2019-2020, Klaus Kähler Holst

  @brief Unit tests

*/

#include <catch2/catch.hpp>

bool True() { return(true); }

TEST_CASE("Target methods", "[target]") {
  
  SECTION("Sanity") {
    CHECK(True());
    CHECK(2 == 2);
  }

  SECTION("dummy1") {
    CHECK(2 == 2);    
  }
}


