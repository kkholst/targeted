#pragma once

#include "env.hpp"

class MiniBatch {
public:
  MiniBatch(int batch_size, int state_action_dim);
  std::vector<double> state_actions;
  std::vector<double> costs;
  std::vector<std::vector<double>> next_states;
  std::vector<std::set<Action>> action_spaces;
  std::vector<bool> terminals;
  int batch_size;
  int state_action_dim;
  torch::Tensor get_batch_x(at::TensorOptions);
  torch::Tensor get_batch_y(vecd, at::TensorOptions);
};

class ReplayMemory {
public:
  ReplayMemory(size_t capacity, size_t batch_size, int state_action_dim);
  void push(vecd &state_action, double cost, vecd next_state,
            std::set<Action> action_space, bool terminal);
  MiniBatch sample();
  size_t size() { return size_; }
  size_t batch_size() { return batch_size_; }
  size_t capacity() { return capacity_; }
  std::random_device rd;

private:
  std::vector<vecd> state_actions_;
  std::vector<double> costs_;
  std::vector<std::vector<double>> next_states_;
  std::vector<bool> terminals_;
  std::vector<std::set<Action>> action_spaces_;
  size_t size_{0};    // the current number of transitions in the replay memory
  size_t capacity_;   // the size of replay memory
  size_t batch_size_; // the size of minibatch
  size_t idx_{0};
  int state_action_dim_;
  std::vector<int> batch_indices_;
};