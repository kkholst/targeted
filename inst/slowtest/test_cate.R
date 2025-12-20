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
if (lava:::versioncheck("polle", c(1, 6)))
test_cate_polle()
