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

test_that("intsurv3", {
})


u <- c(0.5, 1, 1.5)
tau0 <- 1.75

empir_Hu <- function(u) {
  pmin(test_data_unif$TT, tau0)[test_data_unif$TT > u] |> mean()
}

empir_Hu <- Vectorize(empir_Hu)

empir_Hu0 <- empir_Hu(u)

sf <- survfit(Surv(test_data_unif$TT, event = rep(1, nrow(test_data_unif)))~1)
S0 <- summary(sf, times = test_data_unif$TT)$surv

true_int_u_tau_S <- tau0 - u - 1/4*tau0^2 + 1/4 * u^2

intsurv(time = , surv = S0, stop = tau0)

Hu_rmst(time = u, S = S0, tau = tau0)
