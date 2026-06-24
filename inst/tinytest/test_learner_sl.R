set.seed(42)

sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   return(data.frame(y, yb, x1, x2))
}
d <- sim1(1e4)

test_learner_sl <- function() {
  # test with oracle model
  lrs <- list(
    mean = learner_glm(y ~ 1),
    glm = learner_glm(y ~ x1 + x2 + cos(x1)) # oracle for sim1
  )
  lr <- learner_sl(lrs, nfolds = 2)
  lr$estimate(d)

  expect_equal(lr$fit$weights, c(mean = 0, glm = 1))
  # verifies that nfolds argument is passed on to superlearner
  expect_equal(length(lr$fit$folds), 2)

  # default behavior is to return predictions of ensemble model
  expect_equal(length(lr$predict(newdata = sim1(5))), 5)

  # predictions can also be returned for individual learners
  expect_equal(dim(lr$predict(newdata = sim1(5), all.learners = TRUE)), c(5, 2))

  # nfolds can be overwritten in estimate method call
  lr$estimate(d, nfolds = 3)
  expect_equal(length(lr$fit$folds), 3)

  # prediction method fails when one of the base learner's $predict method
  # call fails
  expect_error(
    lr$predict(newdata = data.frame(x1 = 1)),
    pattern = "object 'x2' not found"
  )

  # integration test that fallback.learner argument is correctly handled
  lr_fallback <- learner_sl(lrs, nfolds = 2,
    fallback.learner = learner_glm(y ~ 1)
  )
  lr_fallback$estimate(d)
  preds <- lr_fallback$predict(
    newdata = data.frame(x1 = 1), all.learners = TRUE
  )
  expect_equal(preds[1], preds[2]) # predictions are the same because the
  # fallback.learner is the same as the mean base learner
}
test_learner_sl()
