library("SuperLearner")


set.seed(42)
n <- 200
w <- rnorm(n)
a <- rbinom(n, 1, 0.5)
d <- rbinom(n, 1, plogis(-0.5 + a + 0.5 * w))
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

  fit_pi <- RATE(y ~ d * a, d ~ w, a ~ 1, data = dat, efficient = FALSE)
  expect_true(inherits(fit_pi, "estimate"))
  expect_true(is.finite(coef(fit_pi)[["rate"]]))

  fit_m1 <- RATE(y ~ d * a, d ~ w, a ~ 1, data = dat, M = 1)
  expect_true(inherits(fit_m1, "estimate"))

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
