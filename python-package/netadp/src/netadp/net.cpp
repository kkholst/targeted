#include "net.hpp"

NNet::NNet(int input_dim, int hidden_dim1, int hidden_dim2)
    : input_dim(input_dim), hidden_dim1(hidden_dim1), hidden_dim2(hidden_dim2) {
  // Construct and register two fully connected (fc) Linear submodules
  fc1 = register_module("fc1", torch::nn::Linear(input_dim, hidden_dim1));
  fc2 = register_module("fc2", torch::nn::Linear(hidden_dim1, hidden_dim2));
  fc3 = register_module("fc3", torch::nn::Linear(hidden_dim2, 1));
  // fc1 = register_module("fc1", torch::nn::Linear(input_dim, 1));
  fc1->to(torch::kFloat64);
  fc2->to(torch::kFloat64);
  fc3->to(torch::kFloat64);
}

torch::Tensor NNet::forward(torch::Tensor &x) {
  x = torch::relu(fc1->forward(x.reshape({x.size(0), input_dim})));
  x = torch::relu(fc2->forward(x));
  x = fc3->forward(x);
  // x = fc1->forward(x);
  return x;
}

torch::Tensor NNet::flatten_sveca_tensor(vecd &s, Action &a) {
  // flatten the state and action
  vecd v;
  v.reserve(input_dim);
  v.insert(v.end(), s.begin(), s.end());
  v.insert(v.end(), a.a0_arc_s.begin(), a.a0_arc_s.end());
  v.insert(v.end(), a.a1_arc_k.begin(), a.a1_arc_k.end());
  v.insert(v.end(), a.a1_ser_k.begin(), a.a1_ser_k.end());

  // convert vector to tensor
  return torch::from_blob(v.data(), {1, input_dim}, options).clone();
}

vecd NNet::flatten_sveca(vecd &s, Action &a) {
  // flatten the state and action
  vecd v;
  v.reserve(input_dim);
  v.insert(v.end(), s.begin(), s.end());
  v.insert(v.end(), a.a0_arc_s.begin(), a.a0_arc_s.end());
  v.insert(v.end(), a.a1_arc_k.begin(), a.a1_arc_k.end());
  v.insert(v.end(), a.a1_ser_k.begin(), a.a1_ser_k.end());
  return v;
}

torch::Tensor NNet::to_torch(vecd &sa) {
  return torch::from_blob(sa.data(), {1, input_dim}, options).clone();
}