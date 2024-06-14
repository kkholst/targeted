library(data.table)
library(survival)
library(SuperLearner)

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

test_that("surv_estimating_functions have similar results for type risk or prob."{

  test_survival_models <- fit_survival_models(
    data = test_data_unif,
    response = Surv(time, event) ~ strata(A),
    response_call = "coxph",
    censoring = Surv(time, event == 0) ~ strata(A),
    censoring_call = "coxph"
  )

  test_treatment_model <- fit_treatment_model(data = test_data_unif, treatment = A ~ 1)

  tau0 <- 1

  ## cumhaz(test_survival_models$T_model, newdata = test_data_unif, times = tau0)

  test_risk_treat_ef <- survival_treatment_level_estimating_functions(
    type = "risk",
    data = test_data_unif,
    tau = tau0,
    survival_models = test_survival_models,
    treatment_model = test_treatment_model,
    control = list(sample = 0, blocksize = 0)
  )

  test_prob_treat_ef <- survival_treatment_level_estimating_functions(
    type = "prob",
    data = test_data_unif,
    tau = tau0,
    survival_models = test_survival_models,
    treatment_model = test_treatment_model,
    control = list(sample = 0, blocksize = 0)
  )


})
