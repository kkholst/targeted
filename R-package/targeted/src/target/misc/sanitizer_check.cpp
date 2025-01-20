/*!
  @file sanitizer_check.cpp
  @author Klaus K. Holst
  @copyright 2019-2020, Klaus Kähler Holst

  @brief Sanitizer (UBSAN) example

*/

#include <iostream>

class A {
public:
  bool val;
  A() {};
};

bool val;

int main(int argc, char **argv) {
  A a;
  std::cout << "val=" << a.val << std::endl;
  double res = 2.0/0.0;
  std::cout << "res=" << res << std::endl;
}
