#include "dqn.hpp"

DQN::DQN(rEmat &Time, rEmat &Cost, veci &e_target, veci &hub, veci &OrIdx,
         veci &DeIdx, veci &WFFE, veci &VFFE, vecd &TCrates, vecd &Trans,
         vvi &out_e, int P, int E, int R, int K, int W, int hidden_dim1,
         int hidden_dim2, size_t capacity, size_t batch_size, int Cstep,
         double tau, double lr, double grad_clip, double eps_s, double eps_e,
         double eps_de, int mx_sample, std::string criterion, double max_time,
         double opt_val)
    : Env(Time, Cost, e_target, hub, OrIdx, DeIdx, WFFE, VFFE, TCrates, Trans,
          out_e, P, E, R, K, W),
      policy_net(input_dim, hidden_dim1, hidden_dim2),
      target_net(input_dim, hidden_dim1, hidden_dim2),
      replay_memory(capacity, batch_size, input_dim),
      best_state(P, E, R, K, hub, OrIdx, DeIdx),
      initial_state(P, E, R, K, hub, OrIdx, DeIdx), tmp_action(E, R, K),
      Cstep(Cstep), tau(tau), lr(lr), grad_clip(grad_clip), eps_s(eps_s),
      eps_e(eps_e), eps_de(eps_de), mx_sample(mx_sample), criterion(criterion),
      max_time(max_time), opt_val(opt_val),
      optimizer(policy_net.parameters(), lr) {
  best_state.initialize();
  best_state.total_cost = INFINITY;
  initial_state.initialize();
  initial_visit_K = visit_K;
}

void DQN::update_network() {
  // sample random minibath of transitions (s,a,c,s') from replay memory
  MiniBatch minibatch = replay_memory.sample();

  // get X
  torch::Tensor batch_X = minibatch.get_batch_x(policy_net.options);

  // get y (target value)
  vecd batch_y_vec;
  batch_y_vec.reserve(minibatch.batch_size);
  for (int i = 0; i < minibatch.batch_size; i++) {
    if (minibatch.terminals[i]) {
      batch_y_vec.push_back(minibatch.costs[i]);
    } else {
      double y;
      y = minibatch.costs[i];
      double min_value = INFINITY;
      for (Action a : minibatch.action_spaces[i]) {
        torch::Tensor tensor =
            target_net.flatten_sveca_tensor(minibatch.next_states[i], a);
        double val = target_net.forward(tensor).item<double>();
        if (min_value > val) {
          min_value = val;
        }
      }
      y += min_value;
      batch_y_vec.push_back(y);
    }
  }
  torch::Tensor batch_y =
      minibatch.get_batch_y(batch_y_vec, policy_net.options);
  // Reset gradients
  optimizer.zero_grad();

  // Forward pass
  auto output = torch::smooth_l1_loss(policy_net.forward(batch_X), batch_y);

  // Backward pass
  output.backward();

  // gradient clipping
  torch::nn::utils::clip_grad_value_(policy_net.parameters(), grad_clip);

  // Apply gradients
  optimizer.step();

  // update of the target network's weights
  if (iter_num % Cstep == 0) {
    target_net.parameters() = policy_net.parameters();
  }
}

State DQN::iter(const State &s) {
  // Declaration
  State next_s(P, E, R, K, hub, OrIdx, DeIdx);
  vecd svec = s.flatten();
  vecd state_action_save;
  double min_val = INFINITY;

  // epsiolon-greedy action selection
  std::mt19937 gen(rd());
  std::uniform_real_distribution<double> dis_for_eps(0.0, 1.0);

  if (dis_for_eps(gen) >=
      eps_e + (eps_s - eps_e) * exp(-1 * iter_num / eps_de)) {

    // choose a greedy action
    for (Action a : current_action_set) {
      vecd state_action = policy_net.flatten_sveca(svec, a);
      torch::Tensor tensor = policy_net.to_torch(state_action);
      double value = policy_net.forward(tensor).item<double>();

      if (min_val >= value) {
        min_val = value;
        state_action_save = state_action;
        next_s = step(s, a);
      }
    }
  } else {

    // choose a random action
    std::uniform_int_distribution<int> dis(0, current_action_set.size() - 1);
    int idx = dis(gen);
    auto it = std::begin(current_action_set);
    std::advance(it, idx);
    tmp_action = *it;
    state_action_save = policy_net.flatten_sveca(svec, tmp_action);
    next_s = step(s, tmp_action);
  }

  // keep a record of commodity flows
  seq_loc.push_back(next_s.s3_loc_k);
  seq_ser.push_back(next_s.s4_ser_k);

  // if the given state is a desirable terminal state, then
  // terminate the episode and return the initial state
  if (next_s.check_desirable()) {
    // store transition (s,a,c,s') in replay memory
    replay_memory.push(state_action_save, next_s.cost, next_s.flatten(), {},
                       true);

    // update the neural network
    update_network();

    // increase the iteration number
    iter_num++;

    // increase the episode number
    ep_num++;

    // keep a record of the best solution
    if (next_s.total_cost < best_state.total_cost) {
      best_state = next_s;
      best_loc = seq_loc;
      best_ser = seq_ser;
      best_tot_cost = best_state.total_cost;
      best_ep = ep_num;
      best_iter = iter_num;
      end_time = clock();
      best_time = double(end_time - start_time) / CLOCKS_PER_SEC;
    }
    // initialise the episode:
    // 1) initialise the state
    next_s = initial_state;
    seq_loc = {next_s.s3_loc_k};
    seq_ser = {next_s.s4_ser_k};
    current_action_set = initial_action_set;

    // 2) initialise matrix Z
    visit_K = initial_visit_K;

    return next_s;
  }

  // update the matrix Z
  for (int k : range_K) {
    visit_K(next_s.s3_loc_k[k], k) = true;
  }

  // generate the set of actions given the next state (next_s)
  current_action_set = get_actions(next_s, mx_sample);

  // if the given state is an undesirable terminal state, i.e.,
  // the set of actions is empty, then terminate the episode and
  // return the initial state
  if (current_action_set.empty()) {
    // store transition (s,a,c,s') in replay memory
    replay_memory.push(state_action_save, next_s.cost+1000000, next_s.flatten(), {},
                       true);

    // update the neural network
    update_network();

    // increase the iteration number
    iter_num++;

    // increase the episode number
    ep_num++;

    // initialise the episode:
    // 1) initialise the state
    next_s = initial_state;
    seq_loc = {next_s.s3_loc_k};
    seq_ser = {next_s.s4_ser_k};
    current_action_set = initial_action_set;

    // 2) initialise matrix Z
    visit_K = initial_visit_K;

    return next_s;
  }

  // store transition (s,a,c,s') in replay memory
  replay_memory.push(state_action_save, next_s.cost, next_s.flatten(),
                     current_action_set, false);

  // update the neural network
  update_network();

  return next_s;
}

void DQN::train() {
  // Inisialise the episode
  State s(P, E, R, K, hub, OrIdx, DeIdx);
  s.initialize();
  seq_loc = {s.s3_loc_k};
  seq_ser = {s.s4_ser_k};
  initial_action_set = get_actions(s, mx_sample);
  current_action_set = initial_action_set;

  // Train the model
  if (criterion == "t") {
    start_time = clock();
    while (runtime < max_time) {
      s = iter(s);
      end_time = clock();
      runtime = double(end_time - start_time) / CLOCKS_PER_SEC;
    }
  } else if (criterion == "to") {
    start_time = clock();
    while (runtime <= max_time and (best_state.total_cost - opt_val) > 1e-8) {
      s = iter(s);
      end_time = clock();
      runtime = double(end_time - start_time) / CLOCKS_PER_SEC;
    }
  }
}