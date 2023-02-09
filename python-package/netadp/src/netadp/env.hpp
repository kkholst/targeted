#pragma once

#include <Eigen/Dense>
#include <algorithm>
#include <array>
#include <cmath>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <limits>
#include <numeric>
#include <pybind11/eigen.h>
#include <random>
#include <set>
#include <tuple>
#include <unordered_map>
#include <vector>
#include <torch/torch.h>

namespace py = pybind11;
using rEmat = Eigen::Ref<Eigen::MatrixXd>;
using Emat = Eigen::MatrixXd;
using rEmati = Eigen::Ref<Eigen::MatrixXd>;
using Emati = Eigen::MatrixXd;
using veci = std::vector<int>;
using vecd = std::vector<double>;
using vecp = std::vector<std::pair<int, int>>;
using vvi = std::vector<std::vector<int>>;
using vvp = std::vector<std::vector<std::pair<int, int>>>;

// ACTION
class Action {

public:
  Action(int, int, int);
  veci a0_arc_s;
  veci a1_arc_k;
  veci a1_ser_k;
  bool if_null() const;
  bool operator<(const Action &) const;
  void print_out();

private:
  int E;
  int R;
  int K;
};

// STATE
class State {
private:
  const int P;
  const int E;
  const int R;
  const int K;
  const veci hub;
  const veci OrIdx;
  const veci DeIdx;
  const double tid0 = nextafter(168, 0.0);

public:
  State(int, int, int, int, veci, veci, veci);
  veci s1_loc_s;
  Emat s2_used_cap;
  veci s3_loc_k;
  veci s4_ser_k;
  vecd tau_tid;
  double cost;
  double value;
  double total_cost;
  size_t size = R + E + 2 * K;
  Emati s2_bloc() const;
  Eigen::VectorXd s2_bloc2() const;
  void initialize();
  bool check_desirable() const;
  vecd flatten() const;
  bool operator==(const State &) const;
  State &operator=(const State &);
  void print_out();
};

template <> struct std::hash<State> { size_t operator()(const State &) const; };

// ENVIRONMENT
class Env {

public:
  Env(rEmat &, rEmat &, veci &, veci &, veci &, veci &, veci &, veci &, vecd &,
      vecd &, vvi &, int, int, int, int, int);
  std::random_device rd;
  Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> visit_K;

  // Functions
  State step(const State &, const Action &);
  vvi get_a0(const State &);
  vvp get_a1(const State &);
  bool feasiable(const State &, const Action &);
  std::set<Action> get_actions(const State &, const int);

protected:
  const Emat Time;
  const Emat Cost;
  const veci e_target;
  const veci hub;
  const veci DeIdx;
  const veci OrIdx;
  const veci WFFE;
  const veci VFFE;
  const vecd TCrates;
  const vecd Trans;
  const vvi out_e;
  const int P;
  const int E;
  const int R;
  const int K;
  const int W;
  veci range_R;
  veci range_K;
};