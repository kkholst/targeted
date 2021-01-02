/*!
  @file test_c.cpp
  @author Klaus K. Holst
  @copyright 2019-2021, Klaus Kähler Holst

  @brief Unit tests

*/

#include <doctest/doctest.h>

bool True() { return(true); }

TEST_CASE("Target Sanity Check") {
    REQUIRE(True());
    REQUIRE(2 == 2);
}
