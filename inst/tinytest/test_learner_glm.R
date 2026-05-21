library("tinytest")
library("logger")

set.seed(42)

sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   xf <- factor(rep(c(1, 2), n))
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   return(data.frame(y, yb, x1, x2, xf))
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


capture_warn_logs <- function(expr) {
  msgs <- character(0L)
  old_appender <- eval(
    logger::log_appender(),
    envir = getNamespace("logger")
  )
  on.exit(logger::log_appender(old_appender))
  logger::log_appender(function(line) msgs <<- c(msgs, line))
  force(expr)
  msgs
}

test_insert_nas_when_pred_call_fails <- function() {
  lr <- learner_glm(y ~ x1)
  lr$estimate(d)
  # the default logging threshold is INFO, thus the warning is cast
  msg <- capture_warn_logs(pred <- lr$predict(data.frame(x = 1)))
  expect_true(is.na(pred))
  expect_true(grepl("NAs inserted", msg))
  expect_true(grepl("glm", msg)) # info field is logged correctly
  expect_true(grepl("object 'x1' not found", msg))

  lr$info <- "glmx1"
  msg <- capture_warn_logs(pred <- lr$predict(data.frame(x = 1)))
  expect_true(grepl("glmx1", msg))

  lr$info <- NULL # fallback is implemented when info field is NULL
  msg <- capture_warn_logs(pred <- lr$predict(data.frame(x = 1)))
  expect_true(grepl("learner", msg))

  # test with factors
  lr_factors <- learner_glm(y ~ xf)
  lr_factors$estimate(d)
  msg <- (pred <- lr_factors$predict(data.frame(xf = c(1, 2)))) |>
    capture_warn_logs()

  expect_true(all(is.na(pred)))
  expect_true(grepl("variable 'xf' was fitted with type", msg))

  # only replace failing rows
  msg <- (pred <- lr_factors$predict(data.frame(xf = factor(c(1, 2, 3))))) |>
    capture_warn_logs()
  expect_equal(
    pred,
    c(lr_factors$predict(data.frame(xf = factor(c(1, 2)))), NA)
  )
  expect_true(grepl("factor xf has new level 3", msg))

  # implement additional test to verify that no warning is cast when no
  # NAs are inserted -> though not sure when this can happen with estimate.glm

  # TODO: combination with factor and missing values (quite unlikely to happen)

  # changing log threshold avoids warning
  logger::log_threshold(ERROR)
  msg <- capture_warn_logs(pred <- lr$predict(data.frame(x = 1)))
  expect_equal(length(msg), 0)

  # only replaces NAs for failing rows
  pred <- lr$predict(data.frame(x1 = c(NA, 1)))
  expect_true(is.na(pred[[1]]))
  expect_equal(length(pred), 2)
}
test_insert_nas_when_pred_call_fails()
