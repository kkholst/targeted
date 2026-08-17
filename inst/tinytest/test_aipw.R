## Tests for aipw(): AIPW estimation of a mean with data missing at random.
##
## aipw() delegates to cate(), passing the observation indicator as the
## `treatment.model`. These tests pin that contract down -- in particular
## that cate() still accepts an outcome containing NAs in this setup, where
## the missing rows fall outside the (single) contrast level and therefore
## enter the score with weight zero.

library("tinytest")

sim_mar <- function(n = 2000, seed = 1) {
  set.seed(seed)
  x <- rnorm(n)
  z <- rnorm(n)
  y_full <- 1 + x + 0.5 * z + rnorm(n)      # E[Y] = 1
  r <- rbinom(n, 1, plogis(0.5 + 0.8 * x))  # MAR given x
  data.frame(y = ifelse(r == 1, y_full, NA_real_), x = x, z = z)
}
d <- sim_mar()

test_aipw_runs_with_missing_outcome <- function() {
  ## Regression guard: aipw() substitutes 0 for the missing outcomes before
  ## delegating (those rows carry zero weight in the score), so cate() sees
  ## a complete outcome and its strict NA check does not fire.
  expect_true(any(is.na(d$y)))
  res <- aipw(y ~ x, data = d)
  expect_true(all(!is.na(coef(res$estimate))))
  expect_inherits(res, "cate.targeted")
  ## the outcome handed to cate() is NA-free and zeroed where unobserved
  expect_true(all(!is.na(res$data$y)))
  expect_equal(unname(res$data$y[is.na(d$y), 1]), rep(0, sum(is.na(d$y))))
}
test_aipw_runs_with_missing_outcome()

test_aipw_reserved_columns <- function() {
  ## The internal columns must not silently overwrite user data.
  for (nm in c("R_", "AIPW_Y_")) {
    dd <- d
    dd[[nm]] <- 1
    expect_error(aipw(y ~ x, data = dd), pattern = nm)
  }
}
test_aipw_reserved_columns()

test_aipw_default_propensity <- function() {
  ## When propensity.model is omitted it is built from the response model
  ## RHS with the observation indicator as outcome.
  a1 <- aipw(y ~ x, data = d)
  a2 <- aipw(y ~ x, propensity.model = R_ ~ x, data = d)
  expect_equal(coef(a1$estimate), coef(a2$estimate))
}
test_aipw_default_propensity()

test_aipw_vs_reference <- function() {
  ## Hand-coded AIPW reference for the MAR mean:
  ##   P_n[ R / rho(X) * (Y - Q(X)) + Q(X) ]
  ## with Q fit on the observed rows and a second-order correction for rho.
  r <- !is.na(d$y)
  y0 <- ifelse(is.na(d$y), 0, d$y)
  dr <- transform(d, R_ = r)
  rfit <- glm(R_ ~ x, data = dr, family = binomial)
  rho <- predict(rfit, type = "response")
  qfit <- lm(y ~ x, data = d[r, ])
  q <- predict(qfit, newdata = d)
  k <- (as.integer(r) / rho) * (y0 - q)
  est <- mean(k + q)
  ic <- (k + q) - est
  adj <- -k / rho * rfit$family$mu.eta(rfit$family$linkfun(rho))
  ic <- ic + IC(rfit) %*% colMeans(model.matrix(rfit) * adj)

  res <- aipw(y ~ x, propensity.model = R_ ~ x, data = d)
  expect_equal(unname(coef(res$estimate)), est, tolerance = 1e-10)
  expect_true(max(abs(IC(res$estimate)[, 1] - as.vector(ic))) < 1e-10)
}
test_aipw_vs_reference()

test_aipw_recovers_truth <- function() {
  ## Correctly specified nuisances => approximately unbiased for E[Y] = 1,
  ## and clearly better than the complete-case mean.
  res <- aipw(y ~ x + z, propensity.model = R_ ~ x, data = d)
  est <- unname(coef(res$estimate))
  expect_equal(est, 1, tolerance = 0.1)
  naive <- mean(d$y, na.rm = TRUE)
  expect_true(abs(est - 1) < abs(naive - 1))
}
test_aipw_recovers_truth()

test_aipw_projection_formula <- function() {
  ## `formula` projects the efficient influence function on a design.
  res <- aipw(y ~ x + z, formula = ~ 1 + x, data = d)
  expect_equal(length(coef(res$estimate)), 2L)
  expect_equal(names(coef(res$estimate)), c("(Intercept)", "x"))
}
test_aipw_projection_formula()

test_aipw_no_missing_data <- function() {
  ## With a fully observed outcome the estimator reduces to the sample mean
  ## (the observation indicator is constant, so every unit has weight one).
  set.seed(23)
  n <- 500
  x <- rnorm(n)
  d <- data.frame(y = 1 + x + rnorm(n), x = x)
  res <- suppressWarnings(aipw(y ~ x, data = d))
  expect_equal(unname(coef(res$estimate)), mean(d$y), tolerance = 1e-8)
}
test_aipw_no_missing_data()
