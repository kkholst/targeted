// #pragma once

// #include <Eigen/Dense>
// #include <algorithm>
// #include <array>
// #include <cmath>
// #include <ctime>
// #include <iomanip>
// #include <iostream>
// #include <limits>
// #include <numeric>
// #include <pybind11/eigen.h>
// #include <random>
// #include <set>
// #include <torch/torch.h>
// #include <tuple>
// #include <unordered_map>
// #include <vector>

// using namespace std;
// namespace py = pybind11;

// using rEmat = Eigen::Ref<Eigen::MatrixXd>;
// using Emat = Eigen::MatrixXd;
// using rEmati = Eigen::Ref<Eigen::MatrixXd>;
// using Emati = Eigen::MatrixXd;
// using veci = vector<int>;
// using vecd = vector<double>;
// using vecp = vector<pair<int, int>>;
// using vvi = vector<vector<int>>;
// using vvp = vector<vector<pair<int, int>>>;

// template <typename State, typename V>
// void print_map(std::unordered_map<State, V> const &m) {
//   for (auto it = m.cbegin(); it != m.cend(); ++it) {
//     std::cout << "{" << (*it).first.s1_loc_s << " ["
//               << (*it).first.s2_bloc().reshaped().transpose() << "] "
//               << (*it).first.s3_loc_k << " " << (*it).first.s4_ser_k << ": "
//               << (*it).second << "}\n";
//   }
// }

// // ACTION
// class Action {

// public:
//   Action(int, int, int);
//   veci a0_arc_s;
//   veci a1_arc_k;
//   veci a1_ser_k;
//   bool if_null() const;
//   bool operator<(const Action &) const;
//   void print_out();

// private:
//   int E;
//   int R;
//   int K;
// };

// // STATE
// class State {

// public:
//   State(int, int, int, int, veci, veci, veci);

//   veci s1_loc_s;
//   Emat s2_used_cap;
//   veci s3_loc_k;
//   veci s4_ser_k;
//   vecd tau_tid;
//   double cost = 0;
//   double value = 0;
//   double total_cost = 0;
//   Emati s2_bloc() const;
//   void initialize();
//   bool check_desirable() const;
//   bool operator==(const State &) const;
//   State &operator=(const State &);
//   void print_out();

// private:
//   const int P;
//   const int E;
//   const int R;
//   const int K;
//   const veci hub;
//   const veci OrIdx;
//   const veci DeIdx;
//   const double tid0 = nextafter(168, 0.0);
// };

// template <> struct hash<State> {
//   size_t operator()(const State &) const;
// };

// // ENVIRONMENT
// class Env
// {

// public:
//   Env(rEmat &, rEmat &, veci &, veci &, veci &, veci &, veci &, veci &, vecd &,
//       vecd &, vvi &, int, int, int, int, int);
//   random_device rd;
//   unordered_map<State, double> V;
//   State best;
//   Emati best_cap;
//   vvi seq_loc;
//   vvi seq_ser;
//   vvi best_loc;
//   vvi best_ser;
//   clock_t start_time;
//   clock_t end_time;
//   double runtime = 0;
//   double best_tot_cost = INFINITY;
//   double bellman_min_val = 0;
//   int ep_num = 0;
//   int iter_num = 0;
//   Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> visit_K;
//   Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> initial_visit_K;
//   set<Action> initial_action_set;
//   set<Action> current_action_set;
//   State initial_state;

//   // Functions
//   State update(const State &, const Action &);
//   vvi get_a0(const State &);
//   vvp get_a1(const State &);
//   bool feasiable(const State &, const Action &);
//   set<Action> get_a(const State &, const int, bool);
//   State iter(const State &, const double, const double, const double,
//              const double, const bool, const int);
//   void train(const string, const double, const double, const double,
//              const double, const double, const double, const bool, const int);

// private:
//   const Emat Time;
//   const Emat Cost;
//   const veci e_target;
//   const veci hub;
//   const veci DeIdx;
//   const veci OrIdx;
//   const veci WFFE;
//   const veci VFFE;
//   const vecd TCrates;
//   const vecd Trans;
//   const vvi out_e;
//   const int P;
//   const int E;
//   const int R;
//   const int K;
//   const int W;
//   veci range_R;
//   veci range_K;
// };