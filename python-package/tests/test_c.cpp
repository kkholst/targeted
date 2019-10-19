/*!
  @file test_c.cpp
  @author Klaus K. Holst
  @copyright 2019, Klaus Kähler Holst

  @brief Unit tests

*/

#include <UnitTest++/UnitTest++.h>


bool True() { return(true); }

SUITE(TargetMethods) {
  TEST(Sanity) {
    CHECK(True());
    CHECK_EQUAL(2, 2);
  }

  TEST(dummy1) {
    CHECK(2 == 2);    
  }
  
  TEST(dummy2) {
    // arma_rng::set_seed(1);
    // unsigned n = 5;
    // colvec y(n, 1);   y.randn();
    // colvec w(n, 1);  w.fill(1);
    CHECK_CLOSE(2, 2, 1e-9);
  }
}


int main() {
    return UnitTest::RunAllTests();
}
