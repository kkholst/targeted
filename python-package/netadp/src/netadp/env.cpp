#include "env.hpp"

// A function template for the n-ary cartesian product
template <typename T>
std::vector<std::vector<T>> cartesian_product(std::vector<std::vector<T>> &v) {
  auto product = [](long long a, std::vector<T> &b) { return a * b.size(); };
  const long long N = accumulate(v.begin(), v.end(), 1LL, product);
  std::vector<std::vector<T>> res(N);
  std::vector<T> u(v.size());
  for (long long n = 0; n < N; ++n) {
    lldiv_t q{n, 0};
    for (long long i = v.size() - 1; 0 <= i; --i) {
      q = div(q.quot, v[i].size());
      u[i] = v[i][q.rem];
    }
    res[n] = u;
  }
  return res;
}

// PRINTING
template <typename T>
std::ostream &operator<<(std::ostream &os, const std::vector<T> &v) {
  os << "[";
  for (int i = 0; i < v.size(); ++i) {
    os << v[i];
    if (i != v.size() - 1)
      os << ", ";
  }
  os << "]";
  return os;
}

// ACTION
Action::Action(int E, int R, int K)
    : E(E), R(R), K(K), a0_arc_s(R), a1_arc_k(K), a1_ser_k(K) {}

bool Action::if_null() const {
  return (all_of(a0_arc_s.begin(), a0_arc_s.end(),
                 [this](const int i) { return i >= E; }) &&
          all_of(a1_arc_k.begin(), a1_arc_k.end(),
                 [this](const int i) { return i >= E; }));
}

bool Action::operator<(const Action &a) const {
  return (a.a0_arc_s != this->a0_arc_s || a.a1_arc_k != this->a1_arc_k ||
          a.a1_ser_k != this->a1_ser_k);
}

void Action::print_out() {
  std::cout << std::fixed << std::setprecision(0) << "[" << a0_arc_s << " "
            << a1_arc_k << " " << a1_ser_k << "] ";
}

// STATE
State::State(int P, int E, int R, int K, veci hub, veci OrIdx, veci DeIdx)
    : P(P), E(E), R(R), K(K), hub(hub), OrIdx(OrIdx), DeIdx(DeIdx), s1_loc_s(R),
      s2_used_cap(E + P, R + 1), s3_loc_k(K), s4_ser_k(K, -1),
      tau_tid(R, tid0) {
  double cost = 0;
  double value = 0;
  double total_cost = 0;
}
Emati State::s2_bloc() const { return s2_used_cap.block(0, 1, E, R); }

Eigen::VectorXd State::s2_bloc2() const {
  return s2_used_cap.block(0, 1, E, R).reshaped();
}

void State::initialize() {
  s1_loc_s = hub;
  s2_used_cap = Eigen::MatrixXd::Constant(E + P, R + 1, -INFINITY);
  s2_used_cap.block(0, 1, E, R) = Eigen::MatrixXd::Constant(E, R, -1);
  s3_loc_k = OrIdx;
}

bool State::check_desirable() const {
  return s1_loc_s == hub && s3_loc_k == DeIdx;
}

vecd State::flatten() const {
  std::vector<double> v;
  Eigen::VectorXd tmp = s2_bloc().reshaped();
  v.reserve(size);
  v.insert(v.end(), s1_loc_s.begin(), s1_loc_s.end());
  v.insert(v.end(), tmp.begin(), tmp.end());
  v.insert(v.end(), s3_loc_k.begin(), s3_loc_k.end());
  v.insert(v.end(), s4_ser_k.begin(), s4_ser_k.end());
  return v;
}

bool State::operator==(const State &other) const {
  return (s1_loc_s == other.s1_loc_s && s2_used_cap == other.s2_used_cap &&
          s3_loc_k == other.s3_loc_k && s4_ser_k == other.s4_ser_k);
}

State &State::operator=(const State &other) {
  if (this != &other) {
    this->s1_loc_s = other.s1_loc_s;
    this->s2_used_cap = other.s2_used_cap;
    this->s3_loc_k = other.s3_loc_k;
    this->s4_ser_k = other.s4_ser_k;
    this->tau_tid = other.tau_tid;
    this->cost = other.cost;
    this->value = other.value;
    this->total_cost = other.total_cost;
  }
  return *this;
}

void State::print_out() {
  std::cout << std::fixed << std::setprecision(0) << "[" << s1_loc_s << " ["
            << s2_bloc().reshaped().transpose() << "] " << s3_loc_k << " "
            << s4_ser_k << "]\n";
  // std::cout << tau_tid << std::endl;
}

size_t std::hash<State>::operator()(const State &s) const {
  size_t seed = 0;
  for (int x : s.s1_loc_s) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }
  Emati tmp = s.s2_bloc();
  for (int x : tmp.reshaped()) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }
  for (int x : s.s3_loc_k) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }
  for (int x : s.s4_ser_k) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }
  return seed;
}

Env::Env(rEmat &Time, rEmat &Cost, veci &e_target, veci &hub, veci &OrIdx,
         veci &DeIdx, veci &WFFE, veci &VFFE, vecd &TCrates, vecd &Trans,
         vvi &out_e, int P, int E, int R, int K, int W)
    : Time(Time), Cost(Cost), e_target(e_target), hub(hub), OrIdx(OrIdx),
      DeIdx(DeIdx), WFFE(WFFE), VFFE(VFFE), TCrates(TCrates), Trans(Trans),
      out_e(out_e), P(P), E(E), R(R), K(K), W(W), range_R(R), range_K(K),
      visit_K(P, E) {
  std::iota(range_R.begin(), range_R.end(), 0);
  std::iota(range_K.begin(), range_K.end(), 0);
  visit_K.setZero();
  for (int k : range_K) {
    visit_K(OrIdx[k], k) = true;
  }
}

// Transition function
State Env::step(const State &s, const Action &a) {
  State next_s(P, E, R, K, hub, OrIdx, DeIdx);
  next_s.s2_used_cap = s.s2_used_cap;
  next_s.tau_tid = s.tau_tid;
  next_s.total_cost = s.total_cost;
  next_s.cost = 0;
  // std::cout << "before: " << next_s.cost << std::endl;

  // For each service:
  for (int r : range_R) {
    // update s1
    next_s.s1_loc_s[r] = e_target[a.a0_arc_s[r]];
    // update s2 - add 1 to the used capacities of arcs that are newly selected
    next_s.s2_used_cap(a.a0_arc_s[r], r + 1) += 1;
    // update Tau
    next_s.tau_tid[r] += Time(r, a.a0_arc_s[r]);
    // adding arc-selection costs
    next_s.cost += Cost(r, a.a0_arc_s[r]);
    // std::cout << "after adding arc cost: " << next_s.cost << std::endl;
    // adding deployment costs
    while (next_s.tau_tid[r] >= 168) {
      // adding the deployment cost
      next_s.cost += 180 * TCrates[r];
      // std::cout << "after adding deployment cost: " << next_s.cost << std::endl;
      // update Tau
      next_s.tau_tid[r] -= 168;
    }
  }

  // For each commodity (demand):
  for (int k : range_K) {
    // adding "WeeklyFFE" to the used capacities of arcs serving demands
    next_s.s2_used_cap(a.a1_arc_k[k], a.a1_ser_k[k] + 1) += WFFE[k];
    // adding transit-time cost
    if (a.a1_ser_k[k] != -1) {
      next_s.cost += W * Time(a.a1_ser_k[k], a.a1_arc_k[k]);
    }
    // update s3
    next_s.s3_loc_k[k] = e_target[a.a1_arc_k[k]];
  }
  // update s4
  next_s.s4_ser_k = a.a1_ser_k;

  // adding transshipment costs
  for (int k : range_K) {
    if (s.s4_ser_k[k] != a.a1_ser_k[k] and s.s4_ser_k[k] != -1) {
      next_s.cost += WFFE[k] * Trans[s.s3_loc_k[k]];
    }
  }
  // adding cost at time t to the total cost
  next_s.total_cost += next_s.cost;
  return next_s;
}

vvi Env::get_a0(const State &s) {
  vvi res(R);
  for (int r : range_R) {
    veci myvec(0);
    // Add arcs which have not been selected
    for (int idx : out_e[s.s1_loc_s[r]]) {
      if (s.s2_used_cap(idx, r + 1) == -1) {
        myvec.push_back(idx);
      }
    }
    // Add a loop if service r is at its hub port.
    if (s.s1_loc_s[r] == hub[r]) {
      myvec.push_back(E + hub[r]);
    }
    // Return an empty vector if the given state is an undesirable terminal
    // state
    if (myvec.empty()) {
      return {};
    }
    res[r] = myvec;
  }
  return res;
}

vvp Env::get_a1(const State &s) {
  vvp res(K);
  for (int k : range_K) {
    vecp myvec(0);
    // When commodity k is located at its destination port,
    // choose a loop arc and the same service
    if (s.s3_loc_k[k] == DeIdx[k]) {
      std::pair<int, int> p = {E + s.s3_loc_k[k], s.s4_ser_k[k]};
      myvec.push_back(p);
    }
    // When commodity k is not located at its destination port
    else {
      std::pair<int, int> p = {0, 0};
      for (int idx : out_e[s.s3_loc_k[k]]) {
        // the target node(port) of an arc has not been visited by commodity k
        if (visit_K(e_target[idx], k) == false) {
          for (int r : range_R) {
            // arc should already be selected(constructed) in services and
            if (s.s2_used_cap(idx, r + 1) >= 0) {
              p = {idx, r};
              myvec.push_back(p);
            }
          }
        }
      }
      // a loop is available
      p = {E + s.s3_loc_k[k], s.s4_ser_k[k]};
      myvec.push_back(p);
    }
    res[k] = myvec;
  }
  return res;
}

bool Env::feasiable(const State &s, const Action &a) {
  // If every service & commodity chooses a loop then it's a null action
  if (a.if_null()) {
    return false;
  }
  State s_cp(P, E, R, K, hub, OrIdx, DeIdx);
  s_cp.s2_used_cap = s.s2_used_cap;
  for (int k : range_K) {
    int e = a.a1_arc_k[k];
    int r = a.a1_ser_k[k];
    s_cp.s2_used_cap(e, r + 1) += WFFE[k];
    if (s_cp.s2_used_cap(e, r + 1) > VFFE[r]) {
      return false;
    }
  }
  return true;
}

std::set<Action> Env::get_actions(const State &s, const int max_sample) {
  // if the given state is an undesirable terminal state
  // return an empty set
  vvi vec_a0 = get_a0(s);
  if (vec_a0.empty()) {
    return {};
  }
  // Enumerate all possible a0 choices
  vvp vec_a1 = get_a1(s);
  vvi a0_space = cartesian_product(vec_a0);

  std::set<Action> action_set;
  Action a(E, R, K);

  if (max_sample != 0) {
    // sample a1
    for (veci a0 : a0_space) {
      a.a0_arc_s = a0;
      std::mt19937 gen(rd());
      for (int i = 0; i < max_sample; i++) {
        for (int k : range_K) {
          std::uniform_int_distribution<int> dis(0, vec_a1[k].size() - 1);
          int idx = dis(gen);
          a.a1_arc_k[k] = vec_a1[k][idx].first;
          a.a1_ser_k[k] = vec_a1[k][idx].second;
        }
        if (feasiable(s, a)) {
          action_set.insert(a);
        }
      }
    }
  } else {
    // enumerate all possible a1
    vvp a1_space = cartesian_product(vec_a1);
    for (veci a0 : a0_space) {
      a.a0_arc_s = a0;
      for (std::vector<std::pair<int, int>> a1 : a1_space) {
        for (int k : range_K) {
          a.a1_arc_k[k] = a1[k].first;
          a.a1_ser_k[k] = a1[k].second;
        }
        if (feasiable(s, a)) {
          action_set.insert(a);
        }
      }
    }
  }
  return action_set;
}