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

test_standardize_learner_predictions_emit_warnings <- function() {
  base_pred_fun <- \(...) {warning("some warning"); stats::predict(...)}

  # verify that no warning is logged when no NAs are inserted
  pred_fun <- function(object, newdata, ...) {
    targeted:::standardize_learner_predictions(
      pred.fun = base_pred_fun,
      args = c(list(object = object, newdata = newdata), list(...)),
      model.info = "test-model"
    )

  }
  lr <- learner$new(
    formula = y ~ x1,
    estimate = stats::glm,
    predict = pred_fun
  )
  lr$estimate(d)

  # only log warning from base_pred_fun
  msg <- capture_warn_logs(pred <- lr$predict(data.frame(x1 = c(1, 2))))
  expect_true(grepl("test-model: some warning", msg))

  # log NA warning + warning from base_pred_fun
  msg <- capture_warn_logs(pred <- lr$predict(data.frame(x1 = c(1, NA))))
  expect_true(grepl("some warning", msg))
  expect_true(grepl("NAs inserted", msg))
}
test_standardize_learner_predictions_emit_warnings()
