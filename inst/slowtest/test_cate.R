library("tinytest")
future::plan("multicore")

# comparison against the polle package on CRAN
test_cate_polle <- function() {
  set.seed(1)
  n <- 1000
  x <- rnorm(n)
  a <- rbinom(n, 1, lava::expit(1 + x))
  y <- 1 + a + x - a * x + rnorm(n)
  yb <- rbinom(n, 1, plogis(1 + a + x - a * x))*1.0
  d <- data.frame(yb = yb, y = y, a = a, x = x)


  ## Continuous endpoint
  a <- cate(response.model = learner_glm(y ~ a*x),
            treatment.model = a ~ 1, data=d, mc.cores=1)

  pd <- polle::policy_data(data = data.table::data.table(d),
                           action = "a",
                           covariates = c("x"),
                           utility = "y")
  p1 <- polle::policy_def(1)
  p0 <- polle::policy_def(0)
  a1 <- polle::policy_eval(policy_data = pd,
                           policy = p1,
                           g_models = polle::g_glm( ~ 1),
                           q_models = polle::q_glm( ~ A*x))
  a0 <- polle::policy_eval(policy_data = pd,
                           policy = p0,
                           g_models = polle::g_glm( ~ 1),
                           q_models = polle::q_glm( ~ A*x))

  expect_equivalent(coef(a)["E[y(1)]"], coef(a1), tolerance=1e-4)
  expect_equivalent(coef(a)["E[y(0)]"], coef(a0), tolerance=1e-4)
  expect_equivalent(vcov(a)["E[y(1)]", "E[y(1)]"],
                    vcov(a1)[1], tolerance=1e-4)
  expect_equivalent(vcov(a)["E[y(0)]", "E[y(0)]"],
                    vcov(a0)[1], tolerance=1e-4)


  ## Binary endpoint
  a <- cate(response.model = learner_glm(yb ~ a*x, family=binomial),
            treatment.model = a ~ 1, data=d, mc.cores=1)

  pd <- polle::policy_data(data = data.table::data.table(d),
                           action = "a",
                           covariates = c("x"),
                           utility = "yb")
  p1 <- polle::policy_def(1)
  p0 <- polle::policy_def(0)
  a1 <- polle::policy_eval(policy_data = pd,
                           policy = p1,
                           g_models = polle::g_glm( ~ 1),
                           q_models = polle::q_glm( ~ A*x, family = binomial()))
  a0 <- polle::policy_eval(policy_data = pd,
                           policy = p0,
                           g_models = polle::g_glm( ~ 1),
                           q_models = polle::q_glm( ~ A*x, family = binomial()))

  expect_equivalent(coef(a)["E[yb(1)]"], coef(a1), tolerance=1e-3)
  expect_equivalent(coef(a)["E[yb(0)]"], coef(a0), tolerance=1e-3)
  expect_equivalent(vcov(a)["E[yb(1)]", "E[yb(1)]"],
                    vcov(a1)[1], tolerance=1e-3)
  expect_equivalent(vcov(a)["E[yb(0)]", "E[yb(0)]"],
                    vcov(a0)[1], tolerance=1e-3)

  ## Binary endpoint, propensity-model with covariate
  a <- cate(response.model = learner_glm(yb ~ a*x, family=binomial),
            treatment.model = learner_glm(a ~ x, family=binomial),
            data=d, mc.cores=1)

  pd <- polle::policy_data(data = data.table::data.table(d),
                           action = "a",
                           covariates = c("x"),
                           utility = "yb")
  p1 <- polle::policy_def(1)
  p0 <- polle::policy_def(0)
  a1 <- polle::policy_eval(policy_data = pd,
                           policy = p1,
                           g_models = polle::g_glm( ~ x),
                           q_models = polle::q_glm( ~ A*x, family = binomial()))
  a0 <- polle::policy_eval(policy_data = pd,
                           policy = p0,
                           g_models = polle::g_glm( ~ x),
                           q_models = polle::q_glm( ~ A*x, family = binomial()))

  expect_equivalent(coef(a)["E[yb(1)]"], coef(a1), tolerance=1e-3)
  expect_equivalent(coef(a)["E[yb(0)]"], coef(a0), tolerance=1e-3)
  expect_equivalent(vcov(a)["E[yb(1)]", "E[yb(1)]"],
                    vcov(a1)[1], tolerance=1e-3)
  expect_equivalent(vcov(a)["E[yb(0)]", "E[yb(0)]"],
                    vcov(a0)[1], tolerance=1e-3)

}
if (lava:::versioncheck("polle", geq = c(1, 6)))
test_cate_polle()

test_cate_rep_variance_consistency <- function() {
  # Variance estimates should be consistent across different numbers of
  # repetitions - use simulation to checkthat SE coverage is correct
  set.seed(1)
  nsim <- 500
  n <- 500

  true_ate <- 1
  onerun <- function(n, true_ate, ...) {
    x <- rnorm(n)
    a <- rbinom(n, 1, 0.5)
    y <- true_ate * a + x + rnorm(n)
    d <- data.frame(y = y, a = a, x = x)

    fit1 <- cate(
      y ~ a + x,
      learner_glm(a ~ x, family = binomial),
      calibration.model = ~1,
      nfolds = 5,
      rep = 1,
      data = d
    )

    fit2 <- cate(
      y ~ a + x,
      learner_glm(a ~ x, family = binomial),
      calibration.model = ~1,
      nfolds = 5,
      rep = 5,
      data = d
    )

    ci1 <- parameter(subset(fit1, 3))[,3:4]
    ci2 <- parameter(subset(fit2, 3))[,3:4]
    cover <- c(
      ci1[1] <= true_ate && true_ate <= ci1[2],
      ci2[1] <= true_ate && true_ate <= ci2[2]
    )
    return(cover)
  }

  cover <- future.apply::future_sapply(
    seq(nsim),
    onerun,
    n = n, true_ate = true_ate,
    future.seed = TRUE
  )

  # Both should have coverage close to 95%
  expect_true(abs(mean(cover[1, ]) - 0.95) < 0.05)
  expect_true(abs(mean(cover[2, ]) - 0.95) < 0.05)

  # Coverage should be similar between rep=1 and rep=5
  expect_equal(mean(cover[1, ]), mean(cover[2, ]), tol = 0.05)
}
test_cate_rep_variance_consistency()

# comparison against the AIPW package on CRAN
test_cate_missing_vs_AIPW_package <- function() {
  if (!requireNamespace("AIPW", quietly = TRUE) ||
      !requireNamespace("SuperLearner", quietly = TRUE)) {
    return(invisible(NULL))
  }
  ## AIPW resolves SuperLearner screening algorithms from the search path,
  ## so the package has to be attached rather than merely available.
  suppressMessages(library("SuperLearner"))
  set.seed(1)
  n <- 4000
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  r <- rbinom(n, 1, plogis(0.4 + 0.7 * w1 - 0.3 * w2))
  y_full <- 1 + w1 + 0.5 * w2 + rnorm(n)     # truth: E[Y] = 1
  d <- data.frame(y = ifelse(r == 1, y_full, NA),
                  w1 = w1, w2 = w2, trt = 1L)

  ## targeted::cate with missing.model and a single (degenerate) arm
  tc <- suppressWarnings(
    cate(cate.model = ~1, response.model = y ~ w1 * w2,
         treatment.model = trt ~ 1, missing.model = ~ w1 + w2,
         contrast = 1L, stratify = TRUE, second.order = FALSE, data = d)
  )
  est_cate <- unname(coef(tc)["E[y(1)]"])
  se_cate <- unname(vcov(tc)["E[y(1)]", "E[y(1)]"]^.5)
  ic_cate <- IC(tc)[, "E[y(1)]"]

  ## AIPW package: exposure = observation indicator
  set.seed(1)
  ap <- AIPW::AIPW$new(
    Y = ifelse(is.na(d$y), 0, d$y),
    A = r,
    W = subset(d, select = c(w1, w2)),
    Q.SL.library = "SL.glm.interaction",
    g.SL.library = "SL.glm",
    k_split = 1, verbose = FALSE
  )$fit()
  ap$summary()
  est_aipw <- ap$result["Mean of Exposure", "Estimate"]
  se_aipw <- ap$result["Mean of Exposure", "SE"]
  ## the package reports the uncentred EIF; centre it to match IC()
  ic_aipw <- ap$obs_est$aipw_eif1 - mean(ap$obs_est$aipw_eif1)

  cat(sprintf(paste0("\nAIPW package: est %.6f (targeted %.6f), ",
                     "se %.6f (targeted %.6f), cor(IC) %.7f\n"),
              est_pkg, est_aipw, se_pkg, se_aipw, stats::cor(ic_pkg, ic_aipw)))

  ## point estimate
  expect_equal(est_cate, est_aipw, tolerance = 0.01)
  ## standard error, within 1%
  expect_true(abs(se_aipw / se_cate - 1) < 0.01)
  ## influence functions essentially collinear and of the same scale
  expect_true(stats::cor(ic_cate, ic_aipw) > 0.999)
  expect_equal(est_aipw, 1, tolerance = 0.05)
  expect_equal(est_cate, 1, tolerance = 0.05)
}
test_cate_missing_vs_AIPW_package()

# Monte-Carlo bias and and coverage to validate the influence function
# (including the second-order nuisance corrections) for the setting with missing
# data
sim_mc <- function(n) {
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a  <- rbinom(n, 1, plogis(-0.2 + 0.3 * w1))
  y_full <- 1 + a + w1 + 0.5 * w2 + rnorm(n)   # ATE = 1
  r <- rbinom(n, 1, plogis(0.5 + 0.6 * w1 + 0.5 * a))
  data.frame(y = ifelse(r == 1, y_full, NA_real_), a = a, w1 = w1, w2 = w2)
}

test_cate_missing_coverage <- function() {
  set.seed(1)
  nsim <- 500
  out <- vapply(seq_len(nsim), function(i) {
    d <- sim_mc(1000)
    e <- estimate(cate(cate.model = ~1,
                       response.model = y ~ a * (w1 + w2),
                       treatment.model = a ~ w1 + w2,
                       missing.model  = ~ a * (w1 + w2),
                       data = d))
    ci <- confint(e)["(Intercept)", ]
    c(est = unname(coef(e)["(Intercept)"]),
      cover = as.numeric(ci[1] <= 1 && 1 <= ci[2]))
  }, numeric(2))

  bias <- mean(out["est", ]) - 1
  cover <- mean(out["cover", ])
  ## Monte-Carlo se of the coverage estimate is ~sqrt(.95*.05/400) = 0.011,
  ## so a 4-sd window around the nominal level is [0.906, 0.994].
  cat(sprintf("\nATE bias = %+.4f, coverage = %.3f (nsim = %d)\n",
              bias, cover, nsim))
  expect_true(abs(bias) < 0.02)
  expect_true(cover > 0.91 && cover < 0.99)
}
test_cate_missing_coverage()

test_cate_missing_coverage_stratified <- function() {
  ## stratify = TRUE fits the missing model per arm; coverage is checked
  ## separately because the second-order correction takes a different form.
  set.seed(1)
  nsim <- 500
  out <- vapply(seq_len(nsim), function(i) {
    d <- sim_mc(1000)
    e <- estimate(cate(cate.model = ~1,
                       response.model = y ~ w1 + w2,
                       treatment.model = a ~ w1 + w2,
                       missing.model  = ~ w1 + w2,
                       stratify = TRUE, data = d))
    ci <- confint(e)["(Intercept)", ]
    c(est = unname(coef(e)["(Intercept)"]),
      cover = as.numeric(ci[1] <= 1 && 1 <= ci[2]))
  }, numeric(2))
  bias <- mean(out["est", ]) - 1
  cover <- mean(out["cover", ])
  cat(sprintf("stratified: ATE bias = %+.4f, coverage = %.3f (nsim = %d)\n",
              bias, cover, nsim))
  expect_true(abs(bias) < 0.02)
  expect_true(cover > 0.90 && cover < 0.99)
}
test_cate_missing_coverage_stratified()

sim_mc_misspec <- function(n) {
  ## outcome is strongly non-linear in w1, so a linear working model for
  ## Q is misspecified. In this scenario, the second-order
  ## nuisance corrections actually matter.
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a  <- rbinom(n, 1, plogis(-0.2 + 0.8 * w1))
  y_full <- 1 + a + 2 * sin(3 * w1) + w1^2 + 0.5 * w2 + rnorm(n)  # ATE = 1
  r <- rbinom(n, 1, plogis(0.3 + 0.9 * w1 + 0.4 * a))
  data.frame(y = ifelse(r == 1, y_full, NA_real_), a = a, w1 = w1, w2 = w2)
}

test_cate_missing_coverage_stratified_misspecified <- function() {
  ## Correct treatment and missingness models, misspecified outcome model:
  set.seed(1)
  nsim <- 500
  out <- vapply(seq_len(nsim), function(i) {
    d <- sim_mc_misspec(1000)
    e <- estimate(cate(cate.model = ~1,
                       response.model = y ~ w1 + w2,
                       treatment.model = a ~ w1,
                       missing.model  = ~ w1,
                       stratify = TRUE, data = d))
    ci <- confint(e)["(Intercept)", ]
    c(est = unname(coef(e)["(Intercept)"]),
      cover = as.numeric(ci[1] <= 1 && 1 <= ci[2]))
  }, numeric(2))
  bias <- mean(out["est", ]) - 1
  cover <- mean(out["cover", ])
  cat(sprintf(paste0("stratified + misspecified Q: ATE bias = %+.4f, ",
                     "coverage = %.3f (nsim = %d)\n"), bias, cover, nsim))
  expect_true(abs(bias) < 0.05)
  expect_true(cover > 0.90 && cover < 0.99)
}
test_cate_missing_coverage_stratified_misspecified()
