#pragma once

#include "env.hpp"

struct NNet : torch::nn::Module {
public:
  NNet(int, int, int);
  torch::Tensor forward(torch::Tensor &);
  torch::nn::Linear fc1{nullptr}, fc2{nullptr}, fc3{nullptr};
  const int input_dim;
  const int hidden_dim1;
  const int hidden_dim2;
  at::TensorOptions options = torch::TensorOptions().dtype(torch::kFloat64);
  torch::Tensor flatten_sveca_tensor(vecd &, Action &);
  vecd flatten_sveca(vecd &, Action &);
  torch::Tensor to_torch(vecd &);
};