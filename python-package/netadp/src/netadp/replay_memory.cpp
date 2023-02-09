#include "replay_memory.hpp"

MiniBatch::MiniBatch(int batch_size, int state_action_dim)
    : batch_size(batch_size), state_action_dim(state_action_dim) {
  state_actions.reserve(batch_size * state_action_dim);
  costs.reserve(batch_size);
  next_states.reserve(batch_size);
  action_spaces.reserve(batch_size);
  terminals.reserve(batch_size);
}

torch::Tensor MiniBatch::get_batch_x(at::TensorOptions options) {
  return torch::from_blob(state_actions.data(), {batch_size, state_action_dim},
                          options)
      .clone();
}

torch::Tensor MiniBatch::get_batch_y(vecd y, at::TensorOptions options) {
  return torch::from_blob(y.data(), {batch_size, 1}, options).clone();
}

ReplayMemory::ReplayMemory(size_t capacity, size_t batch_size,
                           int state_action_dim) {
  capacity_ = capacity;
  batch_size_ = batch_size <= capacity ? batch_size : capacity;
  state_action_dim_ = state_action_dim;
  //   batch_indices_.resize(capacity_);
  //   iota(batch_indices_.begin(), batch_indices_.end(), 0);

  state_actions_.reserve(capacity_);
  costs_.reserve(capacity_);
  next_states_.reserve(capacity_);
  action_spaces_.reserve(capacity_);
  terminals_.reserve(capacity_);
}

void ReplayMemory::push(vecd &state_action, double cost, vecd next_state,
                        std::set<Action> action_space, bool terminal) {
  if (size_ < capacity_) {
    state_actions_.push_back(state_action);
    costs_.push_back(cost);
    next_states_.push_back(next_state);
    terminals_.push_back(terminal);
    action_spaces_.push_back(action_space);
    size_ += 1;
  } else {
    state_actions_[idx_] = state_action;
    costs_[idx_] = cost;
    next_states_[idx_] = next_state;
    action_spaces_[idx_] = action_space;
    terminals_[idx_] = terminal;
  }
  idx_ += 1;
  if (idx_ == capacity_) {
    idx_ = 0;
  }
  // size_ = size_ >= capacity_ ? capacity_ : size_ + 1;
}

MiniBatch ReplayMemory::sample() {
  int mini_batch_size = std::min(size_, batch_size_);
  MiniBatch mini_batch(mini_batch_size, state_action_dim_);
  batch_indices_.resize(size_);
  std::iota(batch_indices_.begin(), batch_indices_.end(), 0);

  // Shuffle indices samples
  std::mt19937 gen(rd());
  std::shuffle(std::begin(batch_indices_), std::end(batch_indices_), gen);
  batch_indices_.resize(mini_batch_size);
  for (int i : batch_indices_) {
    mini_batch.state_actions.insert(mini_batch.state_actions.end(),
                                    state_actions_[i].begin(),
                                    state_actions_[i].end());
    mini_batch.costs.push_back(costs_[i]);
    mini_batch.next_states.push_back(next_states_[i]);
    mini_batch.action_spaces.push_back(action_spaces_[i]);
    mini_batch.terminals.push_back(terminals_[i]);
  }

  return mini_batch;
}
