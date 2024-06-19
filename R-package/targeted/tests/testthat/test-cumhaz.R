library(survival)
library(mets)

sim_surv <- function(n, beta, zeta){
  ## id
  id <- 1:n

  ## covariate W and D
  W <- runif(n, min = 1, max = 3)
  D <- as.factor(rbinom(n = n, size = 1, prob = 0.5))

  ## treatment
  A <- rbinom(n = n, size = 1, prob = 0.5)

  ## simulate T
  TT1 <- c(unlist(rexp(n, 1) / exp(matrix(c(rep(1,n), W, rep(1,n), rep(1,n) * W), ncol = 4) %*% beta)))
  TT0 <- c(unlist(rexp(n, 1) / exp(matrix(c(rep(1,n), W), ncol = 2) %*% beta[1:2])))
  TT <- TT1 * A + TT0 * (1-A)

  ## simulate C
  C <- c(unlist(rexp(n, 1) / exp(matrix(c(rep(1,n), A), ncol = 2) %*% zeta)))

  time <- apply(cbind(TT, C), 1, min)
  event <- TT < C

  d <- data.frame(
    W = W,
    D = D,
    A = A,
    TT1 = TT1,
    TT0 = TT0,
    TT = TT,
    C = C,
    time = time,
    event = event
  )
  d <- d[order(time), ]

  return(d)
}

par0 <- list(
  beta = c(-2, 2, -0.2, -0.4),
  zeta = c(-1, 0.5),
  tau = 1
)

set.seed(1)
test_data <- sim_surv(n = 1e3, beta = par0$beta, zeta = par0$zeta)

test_times <- c(0.4, 0.5)
idx <- c(23, 658)
idx_add <- c(idx, c(1, 802))

test_cumhaz_function <- function(object) {

  expect_no_error({
    test_cumhaz <- cumhaz(object, times = test_times, newdata = test_data[idx_add, ])
  })

  expect_equal(
    dim(test_cumhaz$chf),
    c(length(idx_add), length(test_times))
  )

  expect_equal(
    test_cumhaz$time,
    test_times)

  expect_no_error({
    test_cumhaz <- cumhaz(object, times = test_times, newdata = test_data[idx,], individual.time = TRUE)
  })

  test_cumhaz_diag <- cumhaz(object, times = test_times, newdata = test_data[idx,], individual.time = FALSE)

  expect_equal(
    test_cumhaz$chf,
    diag(test_cumhaz_diag$chf)
  )

}

test_that("cumhaz works for model objects of survfit", {

  ## no strata:

  test_survfit <- survfit(Surv(time, event) ~ 1, data = test_data)

  test_cumhaz_function(test_survfit)

  ref_cumhaz <- summary(test_survfit, times = test_times)
  ref_cumhaz <- matrix(rep(ref_cumhaz$cumhaz, nrow(test_data)), ncol = length(test_times), byrow = TRUE)
  colnames(ref_cumhaz) <- test_times

  expect_no_error({
    test_cumhaz <- cumhaz(object = test_survfit, newdata = test_data, times = test_times)
  })

  expect_equal(
    ref_cumhaz,
    test_cumhaz$chf
  )

  ## with strata:

  test_survfit <- survfit(Surv(time, event) ~ A + D, data = test_data)

  test_cumhaz_function(test_survfit)

  ref_cumhaz <- summary(test_survfit, times = test_times)
  ref_cumhaz <- data.table(
    chf = ref_cumhaz$cumhaz,
    strata = ref_cumhaz$strata,
    time = ref_cumhaz$time
  )
  ref_cumhaz <- dcast(ref_cumhaz, strata ~ time, value.var = "chf")
  ref_cumhaz <- merge(
    data.table(strata = strata(test_data[, c("A", "D")])),
    ref_cumhaz,
    by = "strata",
    sort = FALSE,
    all.x = TRUE
  )
  ref_cumhaz <- as.matrix(ref_cumhaz[, -1, drop = FALSE])

  expect_no_error({
    test_cumhaz <- cumhaz(object = test_survfit, newdata = test_data, times = c(0.4, 0.5))
  })

  expect_equal(
    ref_cumhaz,
    test_cumhaz$chf
  )

  ## with factor strata:

  test_survfit <- survfit(Surv(time, event) ~ D, data = test_data)

  test_cumhaz_function(test_survfit)

})


test_that("cumhaz works for model objects of class class phreg", {

  test_phreg <- phreg(Surv(time, event) ~ W, data = test_data)

  test_cumhaz_function(test_phreg)

  test_cumhaz <- cumhaz(test_phreg, newdata = test_data, times= c(0.4, 0.5))
  ref_cumhaz <- as.matrix(predict(test_phreg, times = c(0.4, 0.5))$cumhaz)

  expect_equal(
    ref_cumhaz,
    as.matrix(test_cumhaz$chf)
  )

  test_cumhaz <- cumhaz(test_phreg, newdata = test_data[c(100,800),], times= c(0.4, 0.5))

})


test_that("cumhaz works for model objects of class coxph", {

  library(survival)

  test_coxph <- coxph(Surv(time, event) ~ W, data = test_data)

  test_cumhaz_function(test_coxph)

  test_cumhaz <- cumhaz(test_coxph, newdata = test_data, times = c(0.4, 0.5))

  ref_cumhaz <- survfit(test_coxph, newdata = test_data)
  ref_cumhaz <- summary(ref_cumhaz, times = c(0.4, 0.5))
  ref_cumhaz <- ref_cumhaz$cumhaz |> t()

  expect_equal(
    ref_cumhaz,
    as.matrix(test_cumhaz$chf)
  )

})


test_that("cumhaz works for model objects of class coxph.null", {

  library(survival)

  test_coxph <- coxph(Surv(time, event) ~ strata(A, D), data = test_data)

  test_cumhaz_function(test_coxph)

  ## must be a coxph.null object:
  expect_true({
    inherits(test_coxph, "coxph.null")
  })

  ## survival curve for the coxph.null object:
  ref_cumhaz <- survfit(test_coxph)

  ## cumhaz runs without error for all time points:
  expect_no_error({
    test_cumhaz <- cumhaz(test_coxph, newdata = test_data, times = test_data$time)
  })


  test_predict_1 <- predict(test_coxph, type = "survival") |> unname()
  test_predict_2 <- diag(test_cumhaz$surv) |> unname()

  ## survival predictions for each observation at the individual time points:
  expect_equal(test_predict_1, test_predict_2, tolerance = 10e-15)

  max_time <- test_data$time |> max()
  max_time_epsilon <- max_time + 1

  ## check that the model is not extending predictions:
  expect_error({
    test_cumhaz <- cumhaz(test_coxph, newdata = test_data, times = max_time_epsilon)
  })

  ## check that that the model is extending predictions:
  expect_no_error({
    test_cumhaz <- cumhaz(test_coxph, newdata = test_data, times = max_time_epsilon, extend = TRUE)
  })

  expect_no_error({
    test_cumhaz <- cumhaz(test_coxph, newdata = test_data, times = test_data$time, individual.time = TRUE)
  })

  expect_equal(
    test_predict_1,
    test_cumhaz$surv,
    tolerance = 10e-15
  )

  expect_no_error({
    test_cumhaz <- cumhaz(test_coxph, newdata = test_data, times = rep(1, nrow(test_data)), individual.time = TRUE)
  })

  expect_no_error({
    test_cumhaz <- cumhaz(test_coxph, newdata = test_data, times = c(rep(1, nrow(test_data) - 5), rep(2, 5)), individual.time = TRUE)
  })

  ## test that cumhaz works with nwedata not containing all strata
  test_coxph <- coxph(Surv(time, event) ~ strata(A), data = test_data)

  test_data_0 <- test_data
  test_data_0[, "A"] <- 0

  expect_true({
    !any(is.na(cumhaz(test_coxph, newdata = test_data_0, times = 1)$chf))
  })

})

test_that("cumhaz works for model objects of class class rfsrc.", {

  library(survival)
  library(randomForestSRC)

  test_rfsrc <- rfsrc(Surv(time, event) ~ W, data = test_data, ntree = 10, block.size = 1)

  test_cumhaz_function(test_rfsrc)

  expect_no_error({
    test_cumhaz <- cumhaz(test_rfsrc, newdata = test_data, times = c(0.4, 0.5))
  })

})

test_that("cumhaz works for model objects of class ranger.", {

  library(survival)
  library(ranger)

  test_ranger <- ranger(Surv(time, event) ~ W, data = test_data, num.trees = 10)

  test_cumhaz_function(test_ranger)

  expect_no_error({
    test_cumhaz <- cumhaz(test_ranger, newdata = test_data, times = c(0.4, 0.5))
  })

})
