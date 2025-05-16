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
}
test_superlearner()
