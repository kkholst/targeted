sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   xf <- as.factor(rbinom(n, 1, 0.5))
   y <- x1 + cos(x1) + x2 + x1 * x2 + as.numeric(xf) + rnorm(n, sd = 0.5**.5)
   yb <- as.numeric(y > 0)
   d <- data.frame(y, yb, x1, x2, xf)
   d
}
d0 <- sim1()

test_superlearner <- function() {
  lrs <- list(mean = learner_glm(y ~ 1), glm = learner_glm(y ~ x1))
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
  lr <- learner_glm(y ~ x1 - 1)
  lr$info <- NULL
  sl <- superlearner(list(learner_glm(y ~ 1), lr), data = d0, nfolds = 2)
  expect_equal(names(sl$fit), c("glm", ""))

  # mix of named and unnamed estimators -> use info field for unnamed estimator
  lrs <- list(learner_glm(y ~ 1), lr = lr)
  sl <- superlearner(lrs, data = d0, nfolds = 2)
  expect_equal(names(sl$fit), c("glm", "lr"))

  sl <- superlearner(lrs, data = d0, nfolds = 2, name.prefix = "lr")
  expect_equal(names(sl$fit), c("lr1", "lr"))

  # type checks
  expect_error(
    superlearner(list(\(data) glm(y ~ 1, data = data)), data = d0),
    pattern = "All provided learners must be of class targeted::learner"
  )

  expect_warning(
    superlearner(list(learner_glm(y ~ 1), learner_glm(x1 ~ 1)), data = d0),
    pattern = "Different response variables found among learners: y, x1"
  )

  expect_warning(
    superlearner(list(
      learner_glm(yb ~ 1),
      learner_glm(as.factor(yb) ~ 1, family = "binomial")
    ), data = d0),
    pattern = "Different response variables found among learners: yb, as"
  )

  # verify that superlearner also works with a single learner
  sl <- superlearner(list(glm = learner_glm(y ~ 1)), data = d0, nfolds = 2)
  expect_equal(sl$weights, c(glm = 1))

  sl2 <- superlearner(
    list(glm = learner_glm(y ~ 1), glm = learner_glm(y ~ 1)),
    data = d0, nfolds = 2
  )
  expect_equal(predict(sl, d0), predict(sl2, d0))
}
test_superlearner()

test_predict.superlearner <- function() {
  lrs <- list(mean = learner_glm(y ~ 1), glm = learner_glm(y ~ x1))
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
  lrs <- list(mean = learner_glm(y ~ 1), glm = learner_glm(y ~ x1))
  sl <- superlearner(lrs, data = d0, nfolds = 2)

  expect_equal(weights(sl), sl$weights)
}
test_weights.superlearner()

test_score.superlearner <- function() {
  lrs <- list(mean = learner_glm(y ~ 1), glm = learner_glm(y ~ x1))
  sl <- superlearner(lrs, data = d0, nfolds = 2)
  expect_equal(score(sl), sl$model.score)
}
test_score.superlearner()


test_metalearners <- function() {
  # check edge case where the predictions of two learners are identical
  # set seed to ensure that the predictions in both runs are the same
  set.seed(400)
  d0 <- sim1()
  lrs <- list(
    learner_glm(y ~ I(x1 ** 2) + x2),
    learner_glm(y ~ x1 + x2),
    learner_glm(y ~ x1 + x2)
  )
  set.seed(1)
  sl_quadprog <- superlearner(lrs, data = d0, nfolds = 2)
  set.seed(1)
  sl_nnls <- superlearner(lrs, data = d0, nfolds = 2,
    meta.learner = targeted:::metalearner_nnls2
  )
  set.seed(1)
  sl_convex <- superlearner(lrs, data = d0, nfolds = 2,
    meta.learner = targeted:::metalearner_convexcomb
  )

  # using quadprog::solve.QP splits the weight of the duplicated learner equally
  # whereas nnls::nnls assigns no weight to one learner
  expect_equal(
    sum(sl_quadprog$weights[c(2, 3)]), sum(sl_nnls$weights[c(2, 3)])
  )
  expect_true(0 %in% sl_nnls$weights[c(2, 3)])

  # verify that estimating a convex combination of weights handles duplicated
  # learners correctly
  expect_equal(sum(sl_convex$weights), 1)
  expect_equal(sl_convex$weights[2], sl_convex$weights[3], tol = 1e-3)

  # discrete metalearner handles duplicated learners correctly by selecting only
  # one of the duplicated learners
  set.seed(1)
  sl_discrete <- superlearner(lrs, data = d0, nfolds = 2,
    meta.learner = targeted:::metalearner_discrete
  )
  expect_equal(sum(sl_discrete$weights == 0), 2)
  expect_equal(sum(sl_discrete$weights), 1)
  expect_equal(sl_discrete$weights[which.min(sl_discrete$model.score)][[1]], 1)

  # can also be called with character argument
  set.seed(1)
  sl_discrete_char <- superlearner(lrs, data = d0, nfolds = 2,
  meta.learner = "discrete"
  )
  expect_equal(sl_discrete_char$weights, sl_discrete$weights)

  # nnls::nnls and quadprog::solve.QP should give the same solution
  d0 <- sim1(n = 200)
  lrs <- list(
    learner_glm(y ~ x1 * x2 + cos(x1)),
    learner_glm(y ~ x1 * x2)
  )
  set.seed(1)
  sl_quadprog <- superlearner(lrs, data = d0, nfolds = 2)
  set.seed(1)
  sl_nnls <- superlearner(lrs, data = d0, nfolds = 2,
    meta.learner = targeted:::metalearner_nnls2
  )
  # weights need to be equal when model scores are equal
  expect_equal(sl_quadprog$score, sl_nnls$score)
  expect_equal(sl_quadprog$weights, sl_nnls$weights)
}
test_metalearners()

test_failing_learner <- function() {
  lrs <- list(
    failing = learner_glm(y ~ covar_does_not_exit),
    glm = learner_glm(y ~ x1)
  )
  sl <- superlearner(lrs, data = d0, nfolds = 2)
  lrs$glm$estimate(d0)
  expect_equivalent(
    predict(sl, d0),
    lrs$glm$predict(d0)
  )

  lrs <- list(
    mean = learner_glm(y ~ 1),
    glm = learner_glm(y ~ x1)
  )

  data_failing <- d0
  data_failing[1, "x1"] <- NA
  # glm fails to be estimated for folds that includes the missing value
  sl <- superlearner(lrs, data = data_failing, nfolds = 2)
  expect_equal(weights(sl), c(mean = 1, glm = 0))

  expect_error(
    superlearner(list(
      learner_glm(y ~ covar_does_not_exit)), data = d0, nfolds = 2
    ),
    pattern = "All learners failed to be estimated."
  )

  lr_fail_to_predict <- learner$new(
    y ~ 1, estimate = stats::glm, predict = \(fit, newdata) stop("Some error")
  )
  # verify that learner can be estimated
  lr_fail_to_predict$estimate(d0)
  expect_error(
    superlearner(list(lr_fail_to_predict), data = d0),
    pattern = "hold-out set predictions of all learners contain NAs"
  )
}
test_failing_learner()


test_superlearner_fallback_learner <- function() {
  lrs <- list(learner_glm(y ~ 1), learner_glm(y ~ x1))
  # test different error patterns
  expect_error(
    superlearner(lrs, fallback.learner = y ~ 1, data = d0),
    pattern = "Expecting a fallback.learner of class targeted::learner."
  )

  expect_error( # response variable doesn't exist
    superlearner(lrs, fallback.learner = learner_glm(yy ~ 1), data = d0),
    pattern = "fallback.estimator failed to be estimated."
  )

  lrs <- list(glm.x1 = learner_glm(y ~ x1), glm.xf = learner_glm(y ~ xf))
  fit <- superlearner(lrs, data = d0, nfolds = 2)
  newdata <- data.frame(x1 = 2, xf = factor(3))
  expect_error(
    predict(fit, newdata = newdata),
    pattern = "factor xf has new level 3"
  )
  fit <- superlearner(
    lrs, data = d0, nfolds = 2, fallback.learner = learner_glm(y ~ 1)
  )
  pred <- c(
    fit$fit[["glm.x1"]]$predict(newdata),
    fit$fallback.learner$predict(newdata)
  ) %*% fit$weights
  # fallback.learner is used correctly to make predictions
  expect_equal(predict(fit, newdata = newdata), pred[1, 1])

  # fallback.learner is used even when the base learner could predict
  # successfully for some rows
  nd <- data.frame(x1 = c(2, 2), xf = factor(c(1, 3)))
  preds <- predict(fit, newdata = nd, all.learners = TRUE)
  expect_equal(
    preds[, 2],
    fit$fallback.learner$predict(nd)
  )

  # fallback.learner doesn't recover base learners which produces NA predictions
  nd <- data.frame(x1 = c(2, NA), xf = factor(c(1, 3)))
  expect_true(any(is.na(predict(fit, newdata = nd))))
}
test_superlearner_fallback_learner()

test_SL_defunct <- function() {
  expect_error(SL(), pattern = "defunct")
}
test_SL_defunct()
