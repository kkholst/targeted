#pragma once

#include "env.hpp"

template <typename State, typename V>
void print_map(std::unordered_map<State, V> const &m) {
  for (auto it = m.cbegin(); it != m.cend(); ++it) {
    std::cout << "{" << (*it).first.s1_loc_s << " ["
              << (*it).first.s2_bloc().reshaped().transpose() << "] "
              << (*it).first.s3_loc_k << " " << (*it).first.s4_ser_k << ": "
              << (*it).second << "}\n";
  }
}

class DP : protected Env {

public:
  DP(rEmat &, rEmat &, veci &, veci &, veci &, veci &, veci &, veci &, vecd &,
     vecd &, vvi &, int, int, int, int, int, double, double, double, double,
     int, std::string, double, double);
  std::random_device rd;

  // value function table
  std::unordered_map<State, double> V;

  // learning rate
  const double alpha;

  // epsiolon-greedy method
  const double eps_s;
  const double eps_e;
  const double eps_de;

  // sampling actions
  const int mx_sample;

  // setting stoping criterion
  const std::string criterion;
  const double max_time;
  const double opt_val;

  // recording
  State best_state;
  Emati best_cap;
  vvi best_loc;
  vvi best_ser;
  double best_tot_cost = INFINITY;
  int best_ep;
  int best_iter;
  double best_time;

  vvi seq_loc;
  vvi seq_ser;

  clock_t start_time;
  clock_t end_time;
  double runtime = 0;

  int ep_num = 0;
  int iter_num = 0;
  int Vsize_num = 0;

  // temporary variables
  std::set<Action> current_action_set;

  // fixed ones
  State initial_state;
  std::set<Action> initial_action_set;
  Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> initial_visit_K;

  // functions
  State iter(const State &);
  void train();
};