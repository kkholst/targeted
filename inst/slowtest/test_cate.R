library("tinytest")

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
            propensity.model = a ~ 1, data=d, mc.cores=1)

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
            propensity.model = a ~ 1, data=d, mc.cores=1)

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
            propensity.model = learner_glm(a ~ x, family=binomial),
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

  cover1 <- cover2 <- numeric(nsim)
  true_ate <- 1

  for (i in seq_len(nsim)) {
    x <- rnorm(n)
    a <- rbinom(n, 1, 0.5)
    y <- true_ate * a + x + rnorm(n)
    d <- data.frame(y = y, a = a, x = x)

    fit1 <- cate(y ~ a + x,
                 learner_glm(a ~ x, family = binomial),
                 calibration.model = ~1,
                 nfolds = 5,
                 rep = 1,
                 data = d)

    fit2 <- cate(y ~ a + x,
                 learner_glm(a ~ x, family = binomial),
                 calibration.model = ~1,
                 nfolds = 5,
                 rep = 5,
                 data = d)

    ci1 <- parameter(subset(fit1, 3))[,3:4]
    ci2 <- parameter(subset(fit2, 3))[,3:4]

    cover1[i] <- ci1[1] <= true_ate && true_ate <= ci1[2]
    cover2[i] <- ci2[1] <= true_ate && true_ate <= ci2[2]
  }

  # Both should have coverage close to 95%
  expect_true(abs(mean(cover1) - 0.95) < 0.05)
  expect_true(abs(mean(cover2) - 0.95) < 0.05)

  # Coverage should be similar between rep=1 and rep=5
  expect_true(abs(mean(cover1) - mean(cover2)) < 0.05)
}
test_cate_rep_variance_consistency()
