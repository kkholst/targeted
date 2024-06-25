library(data.table)
library(survival)
library(mets)

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

set.seed(1)
test_data_unif <- sim_surv_unif(n = 1e3)

u <- c(0.5, 1, 1.5)
tau0 <- 1.75



test_that("H_constructor_risk", {

  empir_H_risk <- function(u) {
    (test_data_unif$TT <= tau0)[test_data_unif$TT > u] |> mean()
  }
  empir_H_risk <- Vectorize(empir_H_risk)

  true_H_risk <- function(u) {
    res <- (1 - u / 2) - (1 - tau0 / 2)
    res <- res / (1 - u / 2)
    res <- res * (u <= tau0)
    return(res)
  }
  true_H_risk <- Vectorize(true_H_risk)

  test_survival_models <- fit_survival_models(
    data = test_data_unif,
    response = Surv(time, event) ~ 1,
    censoring = Surv(time, event == 0) ~ 1,
    response_call = "survfit",
    censoring_call = "survfit"
  )

  H <- H_constructor_risk(
    T_model = test_survival_models$T_model,
    tau = tau0,
    individual_time = FALSE
  )

  h <- H(u = u, data = test_data_unif)
  h <- apply(h, 2, mean) |> unname()

  expect_equal(
    h,
    true_H_risk(u),
    tolerance = 4e-2
  )
})


test_that("H_constructor_surv", {
  test_survival_models <- fit_survival_models(
    data = test_data_unif,
    response = Surv(time, event) ~ 1,
    censoring = Surv(time, event == 0) ~ 1,
    response_call = "survfit",
    censoring_call = "survfit"
  )

  true_H_surv <- function(u) {
    res <- (1 - tau0 / 2)
    res <- res / (1 - u / 2)
    res <- res * (u <= tau0) + (u > tau0)
    return(res)
  }
  true_H_surv <- Vectorize(true_H_surv)

  H <- H_constructor_surv(
    T_model = test_survival_models$T_model,
    tau = tau0,
    individual_time = FALSE
  )

  h <- H(u = u, data = test_data_unif)
  h <- apply(h, 2, mean) |> unname()

  expect_equal(
    h,
    true_H_surv(u),
    tolerance = 4e-2
  )

  H <- H_constructor_surv(
    T_model = test_survival_models$T_model,
    tau = tau0,
    individual_time = TRUE
  )

  h <- H(u = u, data = test_data_unif[1:length(u), ])

  expect_equal(
    h,
    true_H_surv(u),
    tolerance = 4e-2
  )
})

test_that("H_constructor_rmst", {
  test_survival_models <- fit_survival_models(
    data = test_data_unif,
    response = Surv(time, event) ~ 1,
    censoring = Surv(time, event == 0) ~ 1,
    response_call = "survfit",
    censoring_call = "survfit"
  )

  true_H_rmst <- function(u) {
    res <- u + (1 - u / 2)^(-1) * (tau0 - u - 1 / 4 * tau0^2 + 1 / 4 * u^2)
    return(res)
  }
  true_H_rmst <- Vectorize(true_H_rmst)

  H <- H_constructor_rmst(
    T_model = test_survival_models$T_model,
    tau = tau0,
    individual_time = FALSE,
    time = test_data_unif$time,
    event = test_data_unif$event
  )

  h <- H(u = u, data = test_data_unif)
  h <- apply(h, 2, mean) |> unname()

  expect_equal(
    h,
    true_H_rmst(u),
    tolerance = 2e-2
  )

  H <- H_constructor_rmst(
    T_model = test_survival_models$T_model,
    tau = tau0,
    individual_time = TRUE,
    time = test_data_unif$time,
    event = test_data_unif$event
  )

  h <- H(u = u, data = test_data_unif[1:length(u), ])

  expect_equal(
    h,
    true_H_rmst(u),
    tolerance = 2e-2
  )
})
