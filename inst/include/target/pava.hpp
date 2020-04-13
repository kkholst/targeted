/*!
  @file utils.hpp
  @author Klaus K. Holst
  @copyright 2020, Klaus Kähler Holst

  @brief Pooled adjacent violator algorithm

*/
#pragma once

#include "utils.hpp"

namespace target {
  
  arma::mat pava(arma::vec y,
		 const arma::vec &x=arma::vec(),
		 arma::vec w=arma::vec());
  
} // namespace target
