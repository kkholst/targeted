#pragma once

template <typename T>
void Print(const std::vector<T> &x, std::string header="", std::string footer="\n") {
  Rcpp::Rcout << header;
  for (auto& i: x) {
    Rcpp::Rcout << i << " ";
  }
  Rcpp::Rcout << footer;
}
