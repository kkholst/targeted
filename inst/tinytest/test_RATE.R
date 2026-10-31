suppressPackageStartupMessages({
  library("mets")
}
)

set.seed(42)
n <- 200
w <- rnorm(n)
a <- rbinom(n, 1, 0.5)
d <- rbinom(n, 1, lava::expit(-0.5 + a + 0.5 * w))
y <- 1 + 2 * a * d + w + rnorm(n)
dat <- data.frame(y, d, a = a, w)


test_RATE <- function() {
  fit <- RATE(
      response = y ~ d * a,
      post.treatment = d ~ w,
      treatment = a ~ 1,
      data = dat, M = 2
    )
  expect_true(inherits(fit, "estimate"))
  cf <- coef(fit)
  expect_true(all(c("a1", "a0", "d", "rate") %in% names(cf)))
  expect_true(is.finite(cf[["rate"]]))

  fit <- RATE(
    response = y ~ a,
    post.treatment = d ~ 1,
    treatment = a ~ 1,
    data = dat, M = 1
  )
  expect_equal(
    fit$coefmat[-4, 1],
    c(
      a1 = mean(subset(dat, a == 1)$y),
      a0 =mean(subset(dat, a == 0)$y),
      d = mean(subset(dat, a == 1)$d)
    )
  )

  fit_pi <- RATE(y ~ d * a, d ~ w, a ~ 1, data = dat, efficient = FALSE)
  expect_true(inherits(fit_pi, "estimate"))
  expect_true(is.finite(coef(fit_pi)[["rate"]]))

  # response/post.treatment accept learner objects directly
  fit_lrn <- RATE(
    response = learner_glm(y ~ d * a),
    post.treatment = learner_sl(list(
      learner_glm(d ~ 1, family = binomial()),
      learner_glm(d ~ w, family = binomial())
    ), nfolds = 2),
    treatment = a ~ 1,
    data = dat, M = 2
  )
  expect_true(inherits(fit_lrn, "estimate"))
  expect_true(is.finite(coef(fit_lrn)[["rate"]]))

  # also works with superlearner for response
  fit_lrn <- RATE(
    response = learner_sl(list(
      learner_glm(y ~ d * a),
      learner_glm(y ~ w * a)

    ), nfolds = 2),
    post.treatment = learner_sl(list(
      learner_glm(d ~ 1, family = binomial()),
      learner_glm(d ~ w, family = binomial())
    ), nfolds = 2),
    treatment = a ~ 1,
    data = dat, M = 2
  )
  expect_true(inherits(fit_lrn, "estimate"))
  expect_true(is.finite(coef(fit_lrn)[["rate"]]))

  # defunct SuperLearner argument interface
  expect_error(
    RATE(y ~ d * a, d ~ w, a ~ 1, data = dat, SL.args.response = list()),
    pattern = "defunct"
  )

  dat_bad_a <- dat
  dat_bad_a$a <- sample(0:2, n, replace = TRUE)
  expect_error(
    RATE(y ~ d * a, d ~ w, a ~ 1, data = dat_bad_a, M = 2),
    pattern = "Expected binary treatment variable"
  )

  dat_bad_d <- dat
  dat_bad_d$d <- dat_bad_d$d + 2
  expect_error(
    RATE(y ~ d * a, d ~ w, a ~ 1, data = dat_bad_d, M = 2),
    pattern = "Expected binary post treatment variable"
    )
}
test_RATE()

sim_surv_rate <- function(n) {
  w <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  d <- rbinom(n, 1, lava::expit(-0.5 + a + 0.5 * w))

  # piecewise-constant baseline cumulative hazard
  cumhaz <- cbind(c(0, 3), c(0, 1))
  rr.event <- exp(-0.5 * a - 0.3 * d + 0.2 * w)

  sim <- mets::simrchaz(cumhaz, rr = rr.event, cens = cumhaz)

  out <- data.frame(
    time = sim$time, event = sim$status,
    a = a, d = d, w = w
  )
  out <- out[order(out$time), ]
  out
}


# TODO: implement additional tests to verify that the function produces
# the correct estimates
# fitphreg <- phreg(
  # Surv(time, event == 1) ~ strata(a), data = sdat
# )
#
# lava::estimate(fitphreg, type = "risk", time = tau)
#
# fitrate <- RATE.surv(
    # response = Surv(time, event) ~ a,
    # post.treatment = d ~ w,
    # treatment = a ~ 1,
    # censoring = Surv(time, event == 0) ~ a,
    # tau = tau,
    # data = sdat,
    # M = 1,
    # call.response = "phreg",
    # call.censoring = "phreg"
  # )

RATE.surv <- targeted:::RATE.surv # because function is currently not exported
# the following tests verify only that the function is able to produce some
# estimates. however, it seems that the estimates are off (see above example)
test_RATE.surv <- function() {
  set.seed(42)
  sdat <- sim_surv_rate(150)
  tau <- as.numeric(median(sdat$time))

  fit <- RATE.surv(
    response = survival::Surv(time, event) ~ d + a + w,
    post.treatment = d ~ w,
    treatment = a ~ 1,
    censoring = survival::Surv(time, event == 0) ~ d + a + w,
    tau = tau,
    data = sdat,
    M = 2,
    pr.treatment = 0.5,
    call.response = "phreg",
    call.censoring = "phreg"
  )
  expect_true(inherits(fit, "estimate"))
  cf <- coef(fit)
  expect_true(all(c("a1", "a0", "d", "rate") %in% names(cf)))
  expect_true(is.finite(cf[["rate"]]))
  expect_true(is.list(attr(fit, "folds")))
  expect_equal(length(attr(fit, "folds")), 2)

  # 2. No cross-fitting (M = 1)
  fit_m1 <- RATE.surv(
    response = survival::Surv(time, event) ~ d + a + w,
    post.treatment = d ~ w,
    treatment = a ~ 1,
    censoring = survival::Surv(time, event == 0) ~ d + a + w,
    tau = tau,
    data = sdat,
    M = 1,
    pr.treatment = 0.5,
    call.response = "phreg",
    call.censoring = "phreg"
  )
  expect_true(inherits(fit_m1, "estimate"))
  expect_true(is.finite(coef(fit_m1)[["rate"]]))
  expect_null(attr(fit_m1, "folds"))

  # 3. Input validation: non-binary treatment
  sdat_bad_a <- sdat
  sdat_bad_a$a <- sample(0:2, nrow(sdat_bad_a), replace = TRUE)
  expect_error(
    RATE.surv(
      response = survival::Surv(time, event) ~ d + a + w,
      post.treatment = d ~ w,
      treatment = a ~ 1,
      censoring = survival::Surv(time, event == 0) ~ d + a + w,
      tau = tau,
      data = sdat_bad_a,
      M = 2,
      pr.treatment = 0.5,
      call.response = "phreg",
      call.censoring = "phreg"
    ),
    pattern = "Expected binary treatment variable"
  )

  # 4. Input validation: non-binary post-treatment marker
  sdat_bad_d <- sdat
  sdat_bad_d$d <- sdat_bad_d$d + 2
  expect_error(
    RATE.surv(
      response = survival::Surv(time, event) ~ d + a + w,
      post.treatment = d ~ w,
      treatment = a ~ 1,
      censoring = survival::Surv(time, event == 0) ~ d + a + w,
      tau = tau,
      data = sdat_bad_d,
      M = 2,
      pr.treatment = 0.5,
      call.response = "phreg",
      call.censoring = "phreg"
    ),
    pattern = "Expected binary post treatment variable"
  )

  # 5. Input validation: data not sorted by time
  sdat_unsorted <- sdat[sample(seq_len(nrow(sdat))), ]
  expect_error(
    RATE.surv(
      response = survival::Surv(time, event) ~ d + a + w,
      post.treatment = d ~ w,
      treatment = a ~ 1,
      censoring = survival::Surv(time, event == 0) ~ d + a + w,
      tau = tau,
      data = sdat_unsorted,
      M = 2,
      pr.treatment = 0.5,
      call.response = "phreg",
      call.censoring = "phreg"
    )
  )
}
test_RATE.surv()
