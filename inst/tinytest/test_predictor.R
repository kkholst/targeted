set.seed(42)

sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   return(data.frame(y, yb, x1, x2))
}
d <- sim1()

simcount <- function(n = 5e2) {
  x <- rnorm(n)
  w <- 50 + rexp(n, rate = 1 / 5)
  y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
  return(data.frame(y, x, w))
}
dcount <- simcount()


test_learner_glm <- function() {
  # basic check that default arguments for learner_glm perform linear
  # regression
  fit_ref <- glm(y ~ x1, data = d)
  lr <- learner_glm(y ~ x1)
  lr$estimate(d)
  expect_equal(coef(lr$fit), coef(fit_ref))

  # poisson regression with offset
  fit_ref <- glm(y ~ x + offset(log(w)), data = dcount, family = poisson)
  lr <- learner_glm(y ~ x + offset(log(w)), family = poisson)
  lr$estimate(dcount)
  expect_equal(coef(lr$fit), coef(fit_ref))

  # default options are to generate predictions on response scale
  newd <- data.frame(x = c(-1, 1), w = c(50, 100))
  expect_equal(lr$predict(newd), predict(fit_ref, newd, type = "response"))

  # predictions can be generated on link scale
  expect_equal(lr$predict(newd, type = "link"), predict(fit_ref, newd))

  # arguments for predict methods can be passed to ml_model in learner_glm
  # call
  lr <- learner_glm(y ~ x + offset(log(w)), family = poisson,
    learner.args = list(predict.args = list(type = "link")),
  )
  lr$estimate(dcount)
  expect_equal(lr$predict(newd), predict(fit_ref, newd))

  # arguments can be again overwritten during method call (unlikely to be used
  # in practice)
  expect_equal(
    lr$predict(newd, type = "response"),
    predict(fit_ref, newd, type = "response")
  )

  # test support for negative binomial regression with MASS
  lr <- learner_glm(y ~ x + offset(log(w)), family = "nb")
  lr$estimate(dcount)
  fit_ref <- MASS::glm.nb(y ~ x + offset(log(w)), data = dcount)
  expect_equal(lr$fit$theta, fit_ref$theta)

  # predict method also works as expected
  expect_equal(lr$predict(newd), predict(fit_ref, newd, type = "response"))
}
test_learner_glm()

# testing the MARS wrapper
test_predictor_mars <- function() {
  m1 <- predictor_mars(y ~ x1 + x2, degree = 1)
  m2 <- predictor_mars(y ~ x1 + x2, degree = 2)

  # Fit models
  m1$estimate(d)
  m2$estimate(d)

  # In-sample model score
  s1 <- scoring(object = m1, newdata = d)[,"mse"]
  s2 <- scoring(object = m2, newdata = d)[,"mse"]

  expect_true(s2 < s1)
}
test_predictor_mars()


# testing the SVR module
test_predictor_svm <- function() {
  # classification / binary prediction
  m1 <- predictor_svm(yb ~ x1 + x2,
                      cost = 1, epsilon = 0.1,
                      probability = TRUE)
  # Fit model
  m1$estimate(d)

  # In-sample predictions
  pr <- m1$predict(d)
  # Check these are actual probabilities
  expect_true(all(pr >= 0 & pr <= 1))

  m2 <- predictor_svm(yb ~ x1 + x2,
                      kernel = "linear",
                      cost = 0.1, epsilon = 1,
                      probability = TRUE)
  m2$estimate(d)
  pr2 <- m2$predict(d)
  # Check we get different results with different hyper-parameters
  expect_true(mean((pr-pr2)^2) > 1e-2)
}
test_predictor_svm()
