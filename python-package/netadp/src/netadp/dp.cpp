#include "dp.hpp"

DP::DP(rEmat &Time, rEmat &Cost, veci &e_target, veci &hub, veci &OrIdx,
       veci &DeIdx, veci &WFFE, veci &VFFE, vecd &TCrates, vecd &Trans,
       vvi &out_e, int P, int E, int R, int K, int W, double alpha,
       double eps_s, double eps_e, double eps_de, int mx_sample,
       std::string criterion, double max_time, double opt_val)
    : Env(Time, Cost, e_target, hub, OrIdx, DeIdx, WFFE, VFFE, TCrates, Trans,
          out_e, P, E, R, K, W),
      best_state(P, E, R, K, hub, OrIdx, DeIdx),
      initial_state(P, E, R, K, hub, OrIdx, DeIdx), alpha(alpha), eps_s(eps_s),
      eps_e(eps_e), eps_de(eps_de), mx_sample(mx_sample), criterion(criterion),
      max_time(max_time), opt_val(opt_val) {
  best_state.total_cost = INFINITY;
  best_state.initialize();
  initial_state.initialize();
  initial_visit_K = visit_K;
}

State DP::iter(const State &s) {
  // Declaration
  State next_s(P, E, R, K, hub, OrIdx, DeIdx);
  double bellman_min_val = INFINITY;

  // Choose action & Solving the Bellman equation

  // Print Out
  // std::cout << std::endl;
  // std::cout << "######################## Solving Bellman equation "
  //              "########################"
  //           << std::endl;

  // epsiolon-greedy action selection
  std::mt19937 gen(rd());
  std::uniform_real_distribution<double> dis_for_eps(0.0, 1.0);

  if (dis_for_eps(gen) >=
      eps_e + (eps_s - eps_e) * exp(-1 * iter_num / eps_de)) {
    // choose the greedy action by solving the Bellman equation
    for (Action a : current_action_set) {
      State tmp = step(s, a);

      // Print Out
      // std::cout << "if we choose a: ";
      // a.print_out();
      // std::cout << " then s': ";
      // tmp.print_out();

      tmp.value = tmp.cost + V[tmp];

      // Print Out
      // std::cout << "cost + V[s'] = " << std::fixed << std::setprecision(1)
      //           << tmp.cost << " + " << V[tmp] << " = " << tmp.value
      //           << std::endl;
      // std::cout << std::endl;

      if (bellman_min_val >= tmp.value) {
        bellman_min_val = tmp.value;
        next_s = tmp;
      }
    }
  } else {
    // choose a random action
    std::uniform_int_distribution<int> dis(0, current_action_set.size() - 1);
    int idx = dis(gen);
    int i = 0;

    for (Action a : current_action_set) {
      State tmp = step(s, a);

      tmp.value = tmp.cost + V[tmp];

      if (i == idx) {
        next_s = tmp;
      }

      if (bellman_min_val >= tmp.value) {
        bellman_min_val = tmp.value;
      }

      i++;
    }
  }

  // Print Out
  // std::cout << "by choosing a greedy action, the next state is: ";
  // next_s.print_out();
  // std::cout << std::endl;

  // update the value function table (default value = 0)

  // Print Out
  // std::cout << std::fixed << std::setprecision(1)
  //           << "Then we update V[s] = " << V[s] << " as follows: ";
  // std::cout << "(1 - " << alpha << ") * " << V[s] << " + " << alpha << " * "
  //           << bellman_min_val << " = ";

  V[s] = (1 - alpha) * V[s] + alpha * bellman_min_val;

  // Print Out
  // std::cout << V[s] << " = New V[s]" << std::endl;

  // increase the iteration number
  iter_num++;

  // keep a record of demand flows
  seq_loc.push_back(next_s.s3_loc_k);
  seq_ser.push_back(next_s.s4_ser_k);

  // if the given state is a desirable terminal state, then
  // terminate the episode and return the initial state
  if (next_s.check_desirable()) {
    // Print Out
    // std::cout << "s is an desirable terminal state." << std::endl;

    // increase the iteration number
    iter_num++;

    // increase the episode number
    ep_num++;

    // keep the record of the best solution
    if (next_s.total_cost < best_state.total_cost) {
      best_state = next_s;
      best_loc = seq_loc;
      best_ser = seq_ser;
      best_tot_cost = best_state.total_cost;
      best_ep = ep_num;
      best_iter = iter_num;
      end_time = clock();
      best_time = double(end_time - start_time) / CLOCKS_PER_SEC;

      // Print Out
      // std::cout << std::endl;
      // std::cout << "######################## Found feasible solution "
      //              "########################"
      //           << std::endl;
      // std::cout << std::endl;
      // std::cout << "(" << ep_num << ", " << iter_num << ") ";
      // std::cout << std::fixed << std::setprecision(10)
      //           << "total cost: " << best_state.total_cost << std::endl;
      // std::cout << "Used Capacity:" << std::endl;
      // std::cout << std::fixed << std::setprecision(0)
      //           << next_s.s2_bloc().transpose() << std::endl;
      // std::cout << "# of visited states: " << V.size() << std::endl;
      // std::cout << "runtime: " << std::fixed << std::setprecision(3)
      //           << double(clock() - start_time) / CLOCKS_PER_SEC <<
      //           std::endl;
    }

    // Print Out
    // std::cout << std::endl;
    // std::cout << "######################## Episode Terminated "
    //              "########################"
    //           << std::endl;
    // std::cout << std::endl;

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

    // Print Out
    // std::cout << "s is a undesirable terminal state." << std::endl;

    // value of undesirable terminal states are infinity
    V[next_s] = INFINITY;

    // Print Out
    // std::cout << std::endl;
    // std::cout << "######################## Episode Terminated "
    //              "########################"
    //           << std::endl;
    // std::cout << std::endl;

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

  return next_s;
}

void DP::train() {
  State s(P, E, R, K, hub, OrIdx, DeIdx);
  //   std::cout << s.tau_tid << std::endl;

  // Inisialise the episode
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
    // std::cout << "Training terminated (t)" << std::endl;

  } else if (criterion == "to") {
    start_time = clock();
    while (runtime <= max_time and (best_state.total_cost - opt_val) > 1e-8) {
      // Print Out
      // std::cout << std::endl;
      // std::cout << "######################## (Episode, Iteration) = (" <<
      // ep_num
      //           << ", " << iter_num << ") ########################"
      //           << std::endl;
      // std::cout << "s = ";
      // s.print_out();

      s = iter(s);
      end_time = clock();
      runtime = double(end_time - start_time) / CLOCKS_PER_SEC;
    }
    // std::cout << "########## Training terminated ##########" << std::endl;
  }
  Vsize_num = V.size();
  best_cap = best_state.s2_bloc().transpose();

  //   // Print the result
  //   std::cout << "episode #: " << ep_num << std::endl;
  //   std::cout << "iteration #: " << iter_num << std::endl;
  //   std::cout << "# of visited states: " << Vsize_num << std::endl;
  //   std::cout << "time: " << std::fixed << std::setprecision(3) << runtime
  //             << std::endl;
}