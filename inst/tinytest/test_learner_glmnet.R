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

simcount <- function(n = 5e2) {
  x <- rnorm(n)
  w <- 50 + rexp(n, rate = 1 / 5)
  y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
  return(data.frame(y, x, w))
}
dcount <- simcount()

test_learner_glmnet <- function() {
  lr <- learner_glmnet_cv(y ~ x1 + x2, nfolds = 3, alpha = 0)
  lr$estimate(d)

  # verify that arguments are passed on to glmnet::cv.glmnet
  expect_equal(lr$fit$call$nfolds, 3)
  expect_equal(lr$fit$call$alpha, 0)

  # verify that predictions are generated for lambda.min
  expect_equal(
    lr$predict(d), lr$predict(d, s = lr$fit$lambda.min)
  )
  # predictions can also be obtained for a range of lambda values
  expect_equal(
    dim(lr$predict(data.frame(x1 = c(0, 1), x2 = 1), s = 1:3)), c(2, 3)
  )

  # user supplied lambda sequence
  .lambda <- c(1e3, 1e2, 1e1)
  lr <- learner_glmnet_cv(y ~ x1 + x2, nfolds = 3, lambda = .lambda)
  lr$estimate(d)
  expect_equal(lr$fit$lambda, .lambda)

  # estimate.args can be overwritten in method call
  lr$estimate(d, nfolds = 4)
  expect_equal(lr$fit$call$nfolds, 4)

  # poisson regression to verify family argument and type argument for predict
  # method + offset
  d0 <- dcount
  d0$xx <- rnorm(nrow(d0))
  lr <- learner_glmnet_cv(y ~ x + xx + offset(log(w)), nfolds = 3,
    family = "poisson"
  )
  lr$estimate(d0)

  # verify that offset is handled correctly
  pr <- lr$predict(data.frame(x = 1, xx = 1, w = c(1, 5)))
  expect_equal(pr[1] * 5, pr[2])

  # predictions on link scale / verifies that family argument is correctly used
  pr_link <- lr$predict(data.frame(x = 1, xx = 1, w = c(1, 5)), type = "link")
  expect_equal(pr, exp(pr_link))
}
test_learner_glmnet()
