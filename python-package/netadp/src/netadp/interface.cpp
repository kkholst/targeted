#include "dp.hpp"
#include "dqn.hpp"
// #include "env.hpp"
// #include "replay_memory.hpp"
#include <memory> // smart pounsigneders (unique_ptr)
#include <pybind11/eigen.h>
#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <target/utils.hpp>

PYBIND11_MODULE(__netadp_c__, m) {
  m.doc() = "Python bindings for the netadp C++ library";
  // m.def("test", &test);

  py::class_<DP>(m, "DP")
      .def(py::init<rEmat &, rEmat &, veci &, veci &, veci &, veci &, veci &,
                    veci &, vecd &, vecd &, vvi &, int, int, int, int, int,
                    double, double, double, double, int, std::string, double,
                    double>())
      .def("train", &DP::train)
      .def_readonly("best_loc", &DP::best_loc)
      .def_readonly("best_ser", &DP::best_ser)
      .def_readonly("best_tot_cost", &DP::best_tot_cost)
      .def_readonly("best_cap", &DP::best_cap)
      .def_readonly("best_ep", &DP::best_ep)
      .def_readonly("best_iter", &DP::best_iter)
      .def_readonly("best_time", &DP::best_time)
      .def_readonly("runtime", &DP::runtime)
      .def_readonly("tot_ep", &DP::ep_num)
      .def_readonly("tot_iter", &DP::iter_num)
      .def_readonly("tot_visited", &DP::Vsize_num);

  py::class_<DQN>(m, "DQN")
      .def(py::init<rEmat &, rEmat &, veci &, veci &, veci &, veci &, veci &,
                    veci &, vecd &, vecd &, vvi &, int, int, int, int, int, int,
                    int, size_t, size_t, int, double, double, double, double,
                    double, double, int, std::string, double, double>())
      .def("train", &DQN::train)
      .def_readonly("best_loc", &DQN::best_loc)
      .def_readonly("best_ser", &DQN::best_ser)
      .def_readonly("best_tot_cost", &DQN::best_tot_cost)
      .def_readonly("best_ep", &DQN::best_ep)
      .def_readonly("best_iter", &DQN::best_iter)
      .def_readonly("best_time", &DQN::best_time)
      .def_readonly("runtime", &DQN::runtime)
      .def_readonly("tot_ep", &DQN::ep_num)
      .def_readonly("tot_iter", &DQN::iter_num);
}
