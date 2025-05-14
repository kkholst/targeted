library("data.table")
library("survival")
library("mets")
int_surv <- targeted:::int_surv
set.seed(42)

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

n <- 2e3
test_data_unif <- sim_surv_unif(n = n)
test_survfit_unif <- survfit(Surv(TT, event = rep(1, n)) ~ 1,
  data = test_data_unif
)
test_times_unif <- sort(test_data_unif$TT)
tau0 <- 1.75
u0 <- 0.5

# test that int_surv() integrates given functions
n <- 1e3
times <- sort(runif(n = n, min = 0, max = 1))

fun <- function(x) 1 - x^2
fun_values <- fun(times)
true_value <- integrate(fun, lower = 0.2, upper = 0.7)$value


value <- int_surv(times = times, surv = fun_values, start = 0.2, stop = 0.7)
expect_true(!is.na(value))
expect_equal(true_value, value, tolerance = 1e-2)

# test that int_surv() matches the true value for a uniform event time
# distribution

surv <- cumhaz(
  test_survfit_unif,
  newdata = test_data_unif,
  times = test_times_unif,
  individual.time = TRUE
)$surv

is <- int_surv(
  times = test_times_unif,
  surv = surv,
  start = u0,
  stop = tau0
)
true_value <- integrate(function(x) 1-x/2, lower = u0, upper = tau0)$value
expect_equal(is, true_value, tolerance = 2e-2)
