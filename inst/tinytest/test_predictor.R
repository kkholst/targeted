set.seed(42)
sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   d <- data.frame(y, yb, x1, x2)
   d
}
d <- sim1()

simcount <- function(n = 5e2) {
  x <- rnorm(n)
  w <- 50 + rexp(n, rate = 1 / 5)
  y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
  return(data.frame(y, x, w))
}
dcount <- simcount()


test_predictor_glm <- function() {
  # basic check that default arguments for predictor_glm perform linear
  # regression
  fit_ref <- glm(y ~ x1, data = d)
  lr <- predictor_glm(y ~ x1)
  lr$estimate(d)

  expect_equal(coef(lr$fit), coef(fit_ref))

  # poisson regression with offset
  fit_ref <- glm(y ~ x + offset(log(w)), data = dcount, family = poisson)
  lr <- predictor_glm(y ~ x + offset(log(w)), family = poisson)
  lr$estimate(dcount)
  expect_equal(coef(lr$fit), coef(fit_ref))

  # default options are to generate predictions on response scale
  newd <- data.frame(x = c(-1, 1), w = c(50, 100))
  expect_equal(lr$predict(newd), predict(fit_ref, newd, type = "response"))

  # predictions can be generated on link scale
  expect_equal(lr$predict(newd, type = "link"), predict(fit_ref, newd))

  # arguments for predict methods can be passed to ml_model in predictor_glm
  # call
  lr <- predictor_glm(y ~ x + offset(log(w)), family = poisson,
    predict.args = list(type = "link"))
  lr$estimate(dcount)
  expect_equal(lr$predict(newd), predict(fit_ref, newd))

  # arguments can be again overwritten during method call (unlikely to be used
  # in practice)
  expect_equal(
    lr$predict(newd, type = "response"),
    predict(fit_ref, newd, type = "response")
  )

  # test support for negative binomial regression with MASS
  lr <- predictor_glm(y ~ x + offset(log(w)), family = "nb")
  lr$estimate(dcount)
  fit_ref <- MASS::glm.nb(y ~ x + offset(log(w)), data = dcount)
  expect_equal(lr$fit$theta, fit_ref$theta)
}
test_predictor_glm()
