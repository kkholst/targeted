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

test_learner_xgboost <- function() {
  params <- list(
    max_depth = 3,
    eta = 0.5,
    subsample = 1.0,
    lambda = 1.0,
    objective = "reg:squarederror"
  )
  args <- c(
    params,
    list(formula = y ~ x1 + x2, nrounds = 3L)
  )

  lr <- do.call(learner_xgboost, args)
  lr$estimate(d)

  # all parameters are passed on correctly
  expect_equal(lr$fit$params[1:length(params)], params)

  # parameters can be overwritten in method call
  lr$estimate(d, eta = 1)
  expect_equal(lr$fit$params$eta, 1)

  # arguments can be passed on to predict s3 function
  pr1 <- lr$predict(head(d)) # use trees from all rounds for predictions
  # use only trees from first 2 boosting rounds for predictions
  pr2 <- lr$predict(head(d), iterationrange = c(1, 2))
  expect_false(all(pr1 == pr2))

  # verify that arguments can be passed on correctly to learner$new
  lr <- learner_xgboost(y ~ ., nrounds = 3,
    learner.args = list(predict.args = list(iterationrange = c(1, 2)))
  )
  lr$estimate(d)
  # iterationrange = NULL is the default for predict.xgb.Booster.
  # this test verifies that 1. learner.args are passed on correctly
  # 2. the supplied predict.args can be overruled in predict method call
  expect_false(
    all(lr$predict(head(d)) == lr$predict(head(d), iterationrange = NULL))
  )

  # test support for multi-class classification / verifies that objective
  # argument is handled correctly
  d0 <- iris
  d0$y <- as.numeric(d0$Species)- 1
  lr <- learner_xgboost(y ~ ., objective = "multi:softprob", num_class = 3)
  lr$estimate(d0)
  expect_equal(dim(lr$predict(d0)), c(nrow(d0), 3))

  # binary classification with binary:logistic
  lr <- learner_xgboost(yb ~ x1 + x2, objective = "binary:logistic")
  lr$estimate(d)
  pr <- lr$predict(head(d))
  expect_true(is.vector(pr))

  # binary classification with objective = "multi:softprob"
  lr <- learner_xgboost(yb ~ x1 + x2, objective = "multi:softprob",
    num_class = 2
  )
  lr$estimate(d)
  # preserve output format of predict.xgb.Booster
  expect_equal(dim(lr$predict(head(d))), c(6, 2))

}
test_learner_xgboost()
