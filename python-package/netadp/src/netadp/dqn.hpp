#pragma once

#include "env.hpp"
#include "net.hpp"
#include "replay_memory.hpp"

class DQN : protected Env {

public:
  DQN(rEmat &, rEmat &, veci &, veci &, veci &, veci &, veci &, veci &, vecd &,
      vecd &, vvi &, int, int, int, int, int, int, int, size_t, size_t, int,
      double, double, double, double, double, double, int, std::string, double,
      double);
  std::random_device rd;

  // DQN
  const int input_dim = (2 + E) * R + 4 * K; // dim(state) + dim(action)
  NNet policy_net;
  NNet target_net;
  ReplayMemory replay_memory;
  const int Cstep;
  const double tau;
  const double lr;
  const double grad_clip;
  torch::optim::SGD optimizer; // SGD optimization algorithm to update our
                               // network's parameters

  // epsilon-greedy method
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

  // temporary variables
  std::set<Action> current_action_set;
  Action tmp_action;

  // fixed ones
  State initial_state;
  std::set<Action> initial_action_set;
  Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> initial_visit_K;

  // functions
  void update_network();
  State iter(const State &);
  void train();
};