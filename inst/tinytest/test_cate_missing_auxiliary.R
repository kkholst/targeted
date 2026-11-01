## cate() with missingness depending on a post-randomization variable Z
## (doubly-robust estimation via `auxiliary.model`)

library("tinytest")

sim_aux <- function(n = 2e4, seed = 1) {
  set.seed(seed)
  w <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  z <- rnorm(n, mean = 0.5 * a, sd = 0.5)
  r <- rbinom(n, 1, plogis(-0.5 - 0.5 * a * z + 0.1 * w))
  y0 <- 1 + 0.5 * a + 2 * z^2 + w + rnorm(n, sd = 0.5)
  y <- ifelse(r == 1, y0, NA_real_)
  data.frame(w, a, z, y0, y)
}
d <- sim_aux()
## Truth: E[Y(1)] = 1 + 0.5 + 2 * (1 + 0.25) = 4, E[Y(0)] = 3, ATE = 1.

test_cate_auxiliary_recovers_ate <- function() {
  ## complete-case analysis
  fit_cc <- cate(y ~ a + w, treatment.model = a ~ 1, data = na.omit(d))
  ## ipw
  fit_ipw <- cate(y ~ a + w, treatment.model = a ~ 1,
                  missing.model = ~ a * z + w, data = d)
  ## efficient with auxillary.model
  fit <- cate(y ~ a + w, treatment.model = a ~ 1,
              missing.model = ~ a * z + w,
              auxiliary.model = y ~ a + I(z^2) + w,
              data = d)

  expect_inherits(fit$auxiliary.model, "learner_glm")
  ## h: list (one entry per replication) of n x n_contrast predictions
  expect_true(is.list(fit$data$h))
  expect_equal(length(fit$data$h), 1L)
  expect_equal(dim(fit$data$h[[1]]), c(nrow(d), 2L))
  expect_true(all(!is.na(fit$data$h[[1]])))

  # cc is biased
  expect_true(abs(coef(fit_cc)["(Intercept)"] - 1) > 0.1)
  # vs augmented estimator
  b <- coef(fit)
  expect_true(abs(b["(Intercept)"] - 1) < 0.1)

  ## augmentation with a correct auxiliary model improves efficiency
  se <- function(x) sqrt(diag(vcov(x$estimate)))["(Intercept)"]
  expect_true(se(fit) < se(fit_ipw))
}
test_cate_auxiliary_recovers_ate()

test_cate_auxiliary_double_robust <- function() {
  ## correct auxiliary model, misspecified missing model
  fit1 <- cate(y ~ a + w, treatment.model = a ~ 1,
               missing.model = ~ a + w,
               auxiliary.model = y ~ a + I(z^2) + w,
               data = d)
  expect_true(abs(coef(fit1)["(Intercept)"] - 1) < 0.1)

  ## misspecified auxiliary model, correct missing model
  fit2 <- cate(y ~ a + w, treatment.model = a ~ 1,
               missing.model = ~ a * z + w,
               auxiliary.model = y ~ a + w,
               pseudo.outcome = "dr",
               data = d)
  expect_true(abs(coef(fit2)["(Intercept)"] - 1) < 0.1)
}
test_cate_auxiliary_double_robust()

test_cate_auxiliary_cv <- function() {
  fit <- cate(y ~ a + w, treatment.model = a ~ 1,
              missing.model = ~ a * z + w,
              auxiliary.model = y ~ a + I(z^2) + w,
              nfolds = 3, silent = TRUE,
              data = d)
  b <- coef(fit)
  expect_true(all(is.finite(b)))
  expect_true(abs(b["(Intercept)"] - 1) < 0.1)
}
test_cate_auxiliary_cv()

test_cate_auxiliary_edge_cases <- function() {
  ## no missing outcomes: auxiliary model is ignored
  fit <- cate(y0 ~ a + w, treatment.model = a ~ 1,
              auxiliary.model = y0 ~ a + I(z^2) + w,
              data = d)
  expect_null(fit$auxiliary.model)
  expect_null(fit$data$h)

  expect_error(
    cate(y ~ a + w, treatment.model = a ~ 1,
         missing.model = ~ a * z + w,
         auxiliary.model = y ~ a + I(z^2) + w,
         calibration.model = ~ w,
         data = d),
    pattern = "Calibration"
  )

  expect_error(
    cate(y ~ a + w, treatment.model = a ~ 1,
         missing.model = ~ a * z + w,
         auxiliary.model = y ~ a + I(z^2) + w,
         pseudo.outcome = "foo",
         data = d)
  )
}
test_cate_auxiliary_edge_cases()
