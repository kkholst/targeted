library("data.table")
library("survival")
library("SuperLearner")
library("mets")
fit_survival_models <- targeted:::fit_survival_models
fit_treatment_model <- targeted:::fit_treatment_model
survival_treatment_level_estimating_functions <- (
  targeted:::survival_treatment_level_estimating_functions
)

sim_surv_unif <- function(n) {
  id <- 1:n
  W <- runif(n, min = -1, max = 1)
  A <- rbinom(n = n, size = 1, prob = 0.5)

  TT <- runif(n, min = 0, max = 2)
  C <- runif(n, min = 0, max = 2)

  time <- apply(cbind(TT, C), 1, min)
  event <- TT < C

  d <- data.frame(id, W, A, TT, C, time, event)[order(time), ]
  return(d)
}

set.seed(1)
test_data_unif <- sim_surv_unif(n = 1e3)

# test surv_estimating_functions have similar results for type risk or surv.
test_survival_models <- fit_survival_models(
  data = test_data_unif,
  response = Surv(time, event) ~ 1,
  response_call = "survfit",
  censoring = Surv(time, event == 0) ~ 1,
  censoring_call = "survfit"
)
test_treatment_model <- fit_treatment_model(data = test_data_unif,
  treatment = A ~ 1
)

test_risk_treat_ef <- survival_treatment_level_estimating_functions(
  type = "risk",
  data = test_data_unif,
  tau = 1,
  survival_models = test_survival_models,
  treatment_model = test_treatment_model,
  control = list(sample = 0, blocksize = 0)
)

test_surv_treat_ef <- survival_treatment_level_estimating_functions(
  type = "surv",
  data = test_data_unif,
  tau = 1,
  survival_models = test_survival_models,
  treatment_model = test_treatment_model,
  control = list(sample = 0, blocksize = 0)
)

expect_equal(
  apply(test_risk_treat_ef$ef, 2, mean),
  apply(1 - test_surv_treat_ef$ef, 2, mean),
  tolerance = 2e-3
)

# surv_estimating_functions returns a consistent estimate for type rmst.
test_survival_models <- fit_survival_models(
  data = test_data_unif,
  response = Surv(time, event) ~ 1,
  response_call = "survfit",
  censoring = Surv(time, event == 0) ~ 1,
  censoring_call = "survfit"
)
test_treatment_model <- fit_treatment_model(data = test_data_unif,
  treatment = A ~ 1
)

tau0 <- 1
true_E_min_T_tau <- (tau0 - 1 / 4 * tau0^2)

test_rmst_treat_ef <- survival_treatment_level_estimating_functions(
  type = "rmst",
  data = test_data_unif,
  tau = tau0,
  survival_models = test_survival_models,
  treatment_model = test_treatment_model,
  control = list(sample = 0, blocksize = 0)
)
est <- apply(test_rmst_treat_ef$ef, 2, mean) |> unname()
expect_equal(est - true_E_min_T_tau, c(0, 0), tolerance = 0.02)

test_rmst_treat_ef_sample <- survival_treatment_level_estimating_functions(
  type = "rmst",
  data = test_data_unif,
  tau = tau0,
  survival_models = test_survival_models,
  treatment_model = test_treatment_model,
  control = list(sample = 100, blocksize = 0)
)
est_sample <- apply(test_rmst_treat_ef_sample$ef, 2, mean) |> unname()
expect_equal(est_sample - true_E_min_T_tau, c(0, 0), tolerance = 0.02)


sim_surv <- function(n, beta, zeta) {
  # id
  id <- 1:n

  # covariate W
  W <- runif(n, min = 1, max = 3)

  # treatment
  A <- rbinom(n = n, size = 1, prob = 0.5)

  # simulate T
  TT1 <- c(
    rexp(n, 1) /
    exp(matrix(c(rep(1, n), W, rep(1, n), rep(1, n) * W), ncol = 4) %*% beta
    )
  )
  TT0 <- c(
    rexp(n, 1) / exp(matrix(c(rep(1, n), W), ncol = 2) %*% beta[1:2])
  )
  TT <- TT1 * A + TT0 * (1 - A)

  # simulate C
  C <- c(unlist(rexp(n, 1) / exp(matrix(c(rep(1, n), A), ncol = 2) %*% zeta)))

  time <- apply(cbind(TT, C), 1, min)
  event <- TT < C

  d <- data.frame(id, W, A, TT1, TT0, TT, C, time, event)[order(time), ]
  return(d)
}

par0 <- list(
  beta = c(-2, 2, -0.2, -0.4),
  zeta = c(-1, 0.5),
  tau = 1
)
set.seed(1)
test_data <- sim_surv(n = 1e3, beta = par0$beta, zeta = par0$zeta)
test_data$D <- rbinom(n = nrow(test_data), size = 1, prob = 0.5)

# test surv_estimating_functions is consistent.
test_survival_models <- fit_survival_models(
  data = test_data,
  response = Surv(time, event) ~ A + A * W,
  response_call = "phreg",
  censoring = Surv(time, event == 0) ~ A,
  censoring_call = "phreg"
)
test_treatment_model <- fit_treatment_model(data = test_data, treatment = A ~ 1)

test_risk_ef <- survival_treatment_level_estimating_functions(
  type = "risk",
  data = test_data,
  tau = par0$tau,
  survival_models = test_survival_models,
  treatment_model = test_treatment_model,
  control = list(sample = 0, blocksize = 0)
)
test_coef <- test_risk_ef$ef |> apply(2, mean)
expect_equal(unname(test_coef), c(0.9399351, 0.8822896), tolerance = 1e-7)
