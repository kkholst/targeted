library("tinytest")

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

test_continuous_response <- function() {
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

  # arguments for predict methods can be passed to learner constructor in
  # learner_glm call
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
test_continuous_response()

test_binary_response <- function() {
  # verifies that the output formats of predict.glm and
  # learner_glm()$predict align
  fit <- glm(yb ~ x1, family = binomial, data = d)

  lr <- learner_glm(yb ~ x1, family = binomial)
  lr$estimate(d)

  expect_equal(fitted(fit), lr$predict(d))
}
test_binary_response()

test_known_issue_with_nse <- function() {
  # test known issue with non-standard evaluations with stats::glm
  fitfun <- function(formula, data, family = gaussian, ...) {
    glm(formula, data = data, family = family, ...)
  }
  expect_error(fitfun(y ~ x1, data = d, weights = NULL))
  reffit <- fitfun(y ~ x1, data = d)

  lr <- learner_glm(y ~ x1, weights = NULL)
  lr$estimate(d)
  expect_equal(coef(reffit), coef(lr$fit))
  # verify that weights argument is captured for estimate.args of learner obj
  expect_null(lr$summary()$estimate.args[["weights"]])
}
test_known_issue_with_nse()

test_fit_call_attribute <- function() {
  # fit$call is rewritten to stay compact: `data`/`family` are stored as
  # symbols rather than the fully-evaluated objects, so print(fit) does not
  # dump the entire dataset

  # --- stats::glm branch ---
  lr <- learner_glm(y ~ x1, family = gaussian())
  lr$estimate(d)
  cl <- lr$fit$call
  expect_true(is.call(cl))
  expect_true(is.symbol(cl[["data"]])) # data kept as symbol
  expect_equal(cl[["data"]], quote(data))

  # the rewritten call is still evaluable and reproduces the model
  data <- d
  family <- gaussian()
  refit <- eval(cl)
  expect_equal(coef(refit), coef(lr$fit))

  # named `...` arguments (specials) are stored as symbols in fit$call (not
  # their evaluated values), keeping print(fit) compact even for e.g. a weights
  # vector.
  d$w <- runif(nrow(d))
  lr <- learner_glm(y ~ x1 + weights(w), family = gaussian(),
    learner.args = list(specials = "weights")
  )
  lr$estimate(d)
  cl <- lr$fit$call
  expect_true("weights" %in% names(cl))
  expect_true(is.symbol(cl[["weights"]]))
  expect_equal(cl[["weights"]], quote(weights))

  fitref <- glm(y ~ x1, data = d, weights = w)
  expect_equal(coef(fitref), coef(lr$fit))
}
test_fit_call_attribute()

