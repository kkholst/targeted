/*!
  @file nondom.hpp
  @author Klaus Kähler Holst
  @copyright 2023, Klaus Kähler Holst

  @brief Non-dominated sets

*/
#pragma once

#include "utils.hpp"

namespace target {

  arma::mat nondominated(const arma::mat &x);

}  // namespace target
