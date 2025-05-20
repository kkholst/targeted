sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
   d <- data.frame(y, x1, x2)
   d
}
d0 <- sim1()

test_superlearner <- function() {
  lrs <- list(mean = predictor_glm(y ~ 1), glm = predictor_glm(y ~ x1))
  sl <- superlearner(lrs, data = d0, nfolds = 2)

  # basic attribute checks
  expect_equal(2, length(sl$folds))
  expect_equal(names(sl$fit), names(lrs))

  # name is derived from info field of learner object
  names(lrs) <- NULL
  sl <- superlearner(lrs, data = d0, nfolds = 2)
  expect_equal(names(sl$fit), c("glm", "glm"))

  # use pre.fix to name learners
  sl <- superlearner(lrs, data = d0, nfolds = 2, name.prefix = "lr")
  expect_equal(names(sl$fit), c("lr1", "lr2"))

  # test info field = NULL
  lr <- predictor_glm(y ~ x1 - 1)
  lr$info <- NULL
  sl <- superlearner(list(predictor_glm(y ~ 1), lr), data = d0, nfolds = 2)
  expect_equal(names(sl$fit), c("glm", ""))

  # mix of named and unnamed estimators -> use info field for unnamed estimator
  lrs <- list(predictor_glm(y ~ 1), lr = lr)
  sl <- superlearner(lrs, data = d0, nfolds = 2)
  expect_equal(names(sl$fit), c("glm", "lr"))

  sl <- superlearner(lrs, data = d0, nfolds = 2, name.prefix = "lr")
  expect_equal(names(sl$fit), c("lr1", "lr"))

  # type checks
  expect_error(
    superlearner(list(\(data) glm(y ~ 1, data = data)), data = d0),
    pattern = "All provided learners must be of class targeted::learner"
  )

  expect_error(
    superlearner(list(predictor_glm(y ~ 1), predictor_glm(x ~ 1)), data = d0),
    pattern = "All learners must have the same response variable"
  )
}
test_superlearner()

test_predict.superlearner <- function() {
  lrs <- list(mean = predictor_glm(y ~ 1), glm = predictor_glm(y ~ x1))
  sl <- superlearner(lrs, data = d0, nfolds = 2)

  # test that names are correctly re-used when predictions for all learners
  # should be returned
  pred <- predict(sl, d0, all.learners = TRUE)
  expect_equal(colnames(pred), names(lrs))
  expect_equal(dim(pred), c(nrow(d0), length(lrs)))

  pred <- predict(sl, d0)
  expect_equal(length(pred), nrow(d0))
}
test_predict.superlearner()

test_weights.superlearner <- function() {
  lrs <- list(mean = predictor_glm(y ~ 1), glm = predictor_glm(y ~ x1))
  sl <- superlearner(lrs, data = d0, nfolds = 2)

  expect_equal(weights(sl), sl$weights)
}
test_weights.superlearner()

test_score.superlearner <- function() {
  lrs <- list(mean = predictor_glm(y ~ 1), glm = predictor_glm(y ~ x1))
  sl <- superlearner(lrs, data = d0, nfolds = 2)
  expect_equal(score(sl), sl$model.score)
}
test_score.superlearner()
