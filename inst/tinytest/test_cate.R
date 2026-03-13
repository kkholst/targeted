library("tinytest")

set.seed(42)
n <- 1000
x <- rnorm(n)
a <- rbinom(n, 1, lava::expit(1 + x))
y <- 1 + a + x - a * x + rnorm(n)
d <- data.frame(y = y, a = a, x = x)

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

test_cate_remainder <- function() {
  # Test seconder order remainder term

  # wrong outcome model
  b <- ate(
    y ~ a,
    nuisance = y ~ a + x,
    propensity = a ~ x,
    adjust.nuisance = FALSE,
    adjust.propensity = TRUE,
    data = d)

  a <- cate(y ~ a + x,
            learner_glm(a ~ x, family=binomial),
            second.order = TRUE,
            data = d)

  expect_equivalent(coef(b), coef(a)[2:1], tolerance=1e-6)
  expect_equivalent(vcov(b), vcov(a)[2:1,2:1], tolerance=1e-5)

  # without second order remainder term correction
  b1 <- ate(
    y ~ a,
    nuisance = y ~ a + x,
    propensity = a ~ x,
    adjust.nuisance = FALSE,
    adjust.propensity = FALSE,
    data = d)

  a1 <- cate(y ~ a + x,
            learner_glm(a ~ x, family=binomial),
            second.order = FALSE,
            data = d)

  expect_equivalent(coef(b1), coef(a1)[2:1], tolerance=1e-6)
  expect_equivalent(vcov(b1), vcov(a1)[2:1,2:1], tolerance=1e-5)

}
test_cate_remainder()

## multiple treatments
n <- 1e3
a <- rbinom(n, 1, 0.5)
a <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
z <- rbinom(n, 1, 0.5)
x <- rnorm(n)
y <- 1*(a==a[1]) + x*(a==a[1]) + rnorm(n, sd=1 + 2*(a==a[1]))
d <- data.frame(a, x, y, z, A = (a == "a") * 1)

test_cate_multiple_treatment <- function() {
  a0 <- cate(y ~ A * x, A ~ 1, data = d)

  a <- cate(y ~ a * x, a ~ 1, data = d)
  expect_true(length(coef(a)) == 6) # 3 exp. potential outcomes, and 3 contrasts

  a2 <- update(a, ~z, data = d)
  expect_true(length(coef(a2)) == 9)

  # check we get same expected potential outcome as with binary treatment
  expect_equivalent(
    coef(a0)["E[y(1)]"],
    coef(a)["E[y(a)]"]
  )
  expect_equivalent(
    vcov(a0)["E[y(1)]","E[y(1)]"],
    vcov(a)["E[y(a)]","E[y(a)]"]
  )
}
test_cate_multiple_treatment()

test_cate_warning <- function() {
  # check we get a warning if the treatment from the propensity.model
  # is not part of the response.model
  expect_warning(
    cate(y ~ a * x, A ~ 1, data = d),
    "treatment variable not present"
  )
}
test_cate_warning()

test_cate_custom_folds <- function() {
  set.seed(7)
  n_obs <- nrow(d)
  idx <- sample(n_obs)
  custom_folds <- split(idx, rep(1:5, length.out = n_obs))
  custom_folds <- lapply(custom_folds, sort)

  res_custom <- cate(y ~ a * x,
                     learner_glm(a ~ x, family = binomial),
                     nfolds = custom_folds,
                     data = d)

  expect_identical(res_custom$folds, custom_folds)

  res_int <- cate(y ~ a * x,
                  learner_glm(a ~ x, family = binomial),
                  nfolds = 5,
                  data = d)
  expect_true(inherits(res_custom, "cate.targeted"))
  expect_equal(length(coef(res_custom)), length(coef(res_int)))

  bad_folds <- list(1:3, 4:6) # doesn't cover 1:n
  expect_error(
    cate(y ~ a * x,
         learner_glm(a ~ x, family = binomial),
         nfolds = bad_folds,
         data = d),
    pattern = "partition"
  )

  expect_warning(
    res_rep <- cate(y ~ a * x,
         learner_glm(a ~ x, family = binomial),
         nfolds = custom_folds,
         rep = 2,
         data = d),
    pattern = "same folds in every repetition"
  )
  expect_null(res_rep$folds) # blank folds attribute when rep > 1
}
test_cate_custom_folds()


test_cate_rep_crossfit <- function() {
  set.seed(1)
  n <- 2000
  x <- rnorm(n)
  a <- rbinom(n, 1, lava::expit(1 + x))
  y <- 1 + a + x - a * x + rnorm(n)
  d <- data.frame(y = y, a = a, x = x)

  # Single cross-fitting reference
  a1 <- cate(y ~ a * x,
             learner_glm(a ~ x, family = binomial),
             nfolds = 5,
             rep = 1,
             data = d)

  # Repeated cross-fitting with nuisance averaging
  a2 <- cate(y ~ a * x,
             learner_glm(a ~ x, family = binomial),
             nfolds = 5,
             rep = 10,
             mc.cores = 1L,
             data = d)

  # Estimates should be similar given large sample-size
  expect_equivalent(coef(a1), coef(a2), tolerance = 0.05)
  expect_equivalent(vcov(a1), vcov(a2), tolerance = 0.05)

}
test_cate_rep_crossfit()

test_cate_rep_calibration_variance <- function() {
  set.seed(1)
  n <- 2000
  x <- rnorm(n)
  a <- rbinom(n, 1, lava::expit(x))
  y <- 1 + a + x - a * x + rnorm(n)
  d <- data.frame(y = y, a = a, x = x)

  a1 <- cate(y ~ a + x,
             learner_glm(a ~ x, family = binomial),
             calibration.model = ~1,
             nfolds = 5,
             rep = 1,
             data = d)

  a2 <- cate(y ~ a + x,
             learner_glm(a ~ x, family = binomial),
             calibration.model = ~1,
             nfolds = 5,
             rep = 10,
             mc.cores = 1,
             data = d)

  # estimates should be similar
  expect_equivalent(coef(a1), coef(a2), tolerance = 0.05)
  expect_equivalent(vcov(a1), vcov(a2), tolerance = 0.05)
}
test_cate_rep_calibration_variance()

test_cate_rep_no_crossfit <- function() {
  # With nfolds=1 (no cross-fitting), rep>1 should not affect variance
  # since there is no fold-specific prediction noise to average out
  set.seed(1)
  n <- 2000
  x <- rnorm(n)
  a <- rbinom(n, 1, lava::expit(x))
  y <- 1 + a + x - a * x + rnorm(n)
  d <- data.frame(y = y, a = a, x = x)

  a1 <- cate(y ~ a + x,
             learner_glm(a ~ x, family = binomial),
             calibration.model = ~1,
             nfolds = 1,
             rep = 1,
             data = d)

  a2 <- cate(y ~ a + x,
             learner_glm(a ~ x, family = binomial),
             calibration.model = ~1,
             nfolds = 1,
             rep = 5,
             mc.cores=1,
             data = d)

  # With nfolds=1, rep>1 should not change estimates or variance
  expect_equivalent(coef(a1), coef(a2), tolerance = 1e-6)
  expect_equivalent(vcov(a1), vcov(a2), tolerance = 1e-6)
}
test_cate_rep_no_crossfit()
