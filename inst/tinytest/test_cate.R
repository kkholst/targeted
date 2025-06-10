library("tinytest")

true_estimate1 <- function(q1) {
  g <- glm(a ~ x, data = d, family = binomial)
  pi <- predict(g, type = "response")
  ic0 <- with(d, a / pi * (y - q1) + q1)
  est <- mean(ic0)
  ic0 <- ic0 - est
  dlinkinv <- g$family$mu.eta
  b <- a / pi^2 * (q1 - y)
  D <- dlinkinv(lava::logit(pi)) * b
  E1 <- colMeans(cbind(D, D * x))
  ic1 <- IC(g) %*% E1
  ic <- ic0 + ic1
  return(estimate(coef = est, IC = ic))
}

simcate <- function(qmod) {
  q1 <- predict(lm(qmod, data = d),
    newdata = transform(d, a = 1)
  )
  e1 <- true_estimate1(q1)
  aa <- cate(
    learner_glm(qmod),
    learner_glm(a ~ x, family = binomial),
    data = d, nfolds = 1
  ) |> estimate()

  expect_equivalent(
    parameter(e1)[1:2],
    parameter(aa)["E[y(1)]", 1:2]
  )
}
simcate(y ~ a * x) # cate: correct q-model
simcate(y ~ a + x) # cate: mis-specified q-model


test_cate_deprecated_arguments <- function() {
  qmod <- y ~ a * x
  q1 <- predict(lm(qmod, data = d), newdata = transform(d, a = 1))
  e1 <- true_estimate1(q1)
  # use deprecated argument names to verify that cate continues to work as
  # expected
  expect_warning(
    aa1 <- cate(
      response_model = learner_glm(qmod),
      propensity.model = learner_glm(a ~ x, family = binomial),
      data = d
    ) |> estimate(),
    pattern = "Please use the `response.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa1)["E[y(1)]", 1:2])

  expect_warning(
    aa2 <- cate(
      response.model = learner_glm(qmod),
      propensity_model = learner_glm(a ~ x, family = binomial),
      data = d
    ) |> estimate(),
    pattern = "Please use the `propensity.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa2)["E[y(1)]", 1:2])

  # user is informed when deprecated treatment argument is used
  expect_warning(aa4 <- cate(
    response.model = learner_glm(qmod),
    propensity.model = learner_glm(a ~ x, family = binomial),
    treatment = ~ 1,
    data = d) |> estimate(),
    pattern = "Please use the `cate.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa4)["E[y(1)]", 1:2])

  # same with cate_model argument
  expect_warning(aa4 <- cate(
    response.model = learner_glm(qmod),
    propensity.model = learner_glm(a ~ x, family = binomial),
    cate_model = ~ 1,
    data = d) |> estimate(),
    pattern = "Please use the `cate.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa4)["E[y(1)]", 1:2])

  # function fails when old treatment arg and new cate.model arg are used
  # together
  expect_error(
    cate(
      response.model = learner_glm(qmod),
      propensity.model = learner_glm(a ~ x, family = binomial),
      cate.model = ~2,
      treatment = ~1,
      data = d
    ),
    pattern = "Calling `cate` with both the obsolete"
  )
}
test_cate_deprecated_arguments()


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

  pd <- polle::policy_data(data = data.table(d),
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

  pd <- polle::policy_data(data = data.table(d),
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

  pd <- polle::policy_data(data = data.table(d),
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
test_cate_polle()


test_cate_ate <- function() {
  set.seed(1)
  n <- 1000
  x <- rnorm(n)
  a <- rbinom(n, 1, lava::expit(1 + x))
  y <- 1 + a + x - a * x + rnorm(n)
  yb <- rbinom(n, 1, plogis(1 + a + x - a * x))*1.0
  d <- data.frame(yb = yb, y = y, a = a, x = x)

  a <- cate(response.model = learner_glm(yb ~ a*x, family=binomial()),
            propensity.model = learner_glm(a ~ x, family=binomial()),
            data=d, mc.cores=1)

  at <- ate(yb ~ a, nuisance = ~a*x, propensity = ~x, family=binomial(), data=d)

  expect_equivalent(coef(a)["E[yb(1)]"], coef(at)["a=1"], tolerance=1e-3)
  expect_equivalent(coef(a)["E[yb(0)]"], coef(at)["a=0"], tolerance=1e-3)

  # not exactly the same standard errors, as the 'ate' function
  # creates a correction for estimation of both nuisance models
  expect_equivalent(vcov(a)["E[yb(1)]","E[yb(1)]"],
                    vcov(at)["a=1","a=1"], tolerance=1e-2)
  expect_equivalent(vcov(a)["E[yb(0)]","E[yb(0)]"],
                    vcov(at)["a=0","a=0"], tolerance=1e-2)
}
test_cate_ate()
