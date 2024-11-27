library("data.table")
library("survival")
library("SuperLearner")
library("mets")
set.seed(4242)

sim_surv_unif <- function(n) {
  id <- 1:n
  W <- runif(n, min = -1, max = 1)
  A <- rbinom(n = n, size = 1, prob = 0.5)

  TT <- runif(n, min = 0, max = 2)
  C <- runif(n, min = 0, max = 2)

  time <- apply(cbind(TT, C), 1, min)
  event <- TT < C

  d <- data.frame(
    id = id,
    W = W,
    A = A,
    TT = TT,
    C = C,
    time = time,
    event = event
  )
  d <- d[order(time), ]
}


test_data_unif <- sim_surv_unif(n = 1e3)

# test rcai has mean zero for type = 'risk' and type = 'surv'.

test_survival_models <- fit_survival_models(
  data = test_data_unif,
  response = Surv(time, event) ~ 1,
  response_call = "survfit",
  censoring = Surv(time, event == 0) ~ 1,
  censoring_call = "survfit"
)
tau0 <- 1

risk_rcai <- rcai(
  T_model = test_survival_models$T_model,
  C_model = test_survival_models$C_model,
  data = test_data_unif,
  time = test_data_unif$time,
  event = test_data_unif$event,
  tau = tau0,
  H_constructor = H_constructor_risk,
  sample = 0,
  blocksize = 0,
  return_all = TRUE
)
expect_equal(mean(risk_rcai$Nc - risk_rcai$Lc), 0, tolerance = 1e-10)

surv_rcai <- rcai(
  T_model = test_survival_models$T_model,
  C_model = test_survival_models$C_model,
  data = test_data_unif,
  time = test_data_unif$time,
  event = test_data_unif$event,
  tau = tau0,
  H_constructor = H_constructor_surv,
  sample = 0,
  blocksize = 0,
  return_all = TRUE
)
expect_equal(mean(surv_rcai$Nc - surv_rcai$Lc), 0, tolerance = 1e-10)

# test rcai has mean zero for type = 'rmst'
test_survival_models <- fit_survival_models(
    data = test_data_unif,
    response = Surv(time, event) ~ 1,
    response_call = "survfit",
    censoring = Surv(time, event == 0) ~ 1,
    censoring_call = "survfit"
  )
tau0 <- 1

rmst_rcai <- rcai(
  T_model = test_survival_models$T_model,
  C_model = test_survival_models$C_model,
  data = test_data_unif,
  time = test_data_unif$time,
  event = test_data_unif$event,
  tau = tau0,
  H_constructor = H_constructor_rmst,
  sample = 0,
  blocksize = 0,
  return_all = TRUE
)
expect_equal(mean(rmst_rcai$Nc - rmst_rcai$Lc), 0, tolerance = 1e-10)


sim_surv <- function(n, beta, zeta) {
  # id
  id <- 1:n

  # covariate W
  W <- runif(n, min = 1, max = 3)

  # treatment
  A <- rbinom(n = n, size = 1, prob = 0.5)

  # simulate T
  # TT1 <- c(unlist(rexp(n, 1) / exp(matrix(c(rep(1, n), W, rep(1, n), rep(1, n) * W), ncol = 4) %*% beta)))
  TT1 <- c(
    rexp(n, 1) /
    exp(matrix(c(rep(1, n), W, rep(1, n), rep(1, n) * W), ncol = 4) %*% beta
    )
  )
  # TT0 <- c(unlist(rexp(n, 1) / exp(matrix(c(rep(1, n), W), ncol = 2) %*% beta[1:2])))
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
test_data <- sim_surv(n = 1e3, beta = par0$beta, zeta = par0$zeta)
test_data$D <- rbinom(n = nrow(test_data), size = 1, prob = 0.5)
