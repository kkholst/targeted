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

  # base learners can be overwritten in estimate method call
  lr$estimate(d, learners = list(glm2 = learner_glm(y ~ x1)))
  expect_equal(names(lr$fit$fit), "glm2")
}
test_learner_sl()

# deparsed formulas of the base learners of a learnerSL object
base_formulas <- function(sl) {
  vapply(sl$learners, \(lr) deparse(lr$formula), character(1))
}

test_learner_sl_class <- function() {
  lrs <- list(
    mean = learner_glm(y ~ 1),
    glm = learner_glm(y ~ x1 + x2 + cos(x1))
  )
  lr <- learner_sl(lrs, nfolds = 2)
  expect_equal(class(lr), c("learner_sl", "learner", "R6"))
  expect_true(
    inherits(learnerSL$new(estimate.args = list(learners = lrs)), "learner_sl")
  )

  # a single learner is returned as is
  expect_identical(learner_sl(lrs$glm), lrs$glm)

  # formula is the formula of the first base learner
  expect_equal(deparse(lr$formula), "y ~ 1")
  expect_identical(environment(lr$formula), environment(lrs$mean$formula))
  lr <- learner_sl(rev(lrs))
  expect_equal(deparse(lr$formula), "y ~ x1 + x2 + cos(x1)")

  # base learners
  lr <- learner_sl(lrs, nfolds = 2)
  expect_equal(names(lr$learners), c("mean", "glm"))
  expect_identical(lr$opt("learners"), lr$learners)
  expect_identical(lr$summary()$estimate.args$learners, lr$learners)
  expect_equal(lr$opt("nfolds"), 2)
  expect_error(lr$learners <- list(), pattern = "read-only")

  # base learners are copies of the provided learners
  expect_false(identical(lr$learners$glm, lrs$glm))

  # input validation
  expect_error(
    learner_sl(list(learner_glm(y ~ x1), learner_glm(yb ~ x1))),
    pattern = "same response variable"
  )
  expect_error(learner_sl(list()), pattern = "non-empty list")
  expect_error(learner_sl(list(y ~ x1)), pattern = "non-empty list")
  lr_noformula <- learner$new(estimate = function(y, x) lm.fit(x = x, y = y))
  expect_error(
    learner_sl(list(lr_noformula)),
    pattern = "formula with a response"
  )
}
test_learner_sl_class()

test_learner_sl_new <- function() {
  lrs <- list(
    mean = learner_glm(y ~ 1),
    glm = learner_glm(y ~ x1 + x2)
  )

  # same interface as learner$new()
  expect_identical(
    names(formals(learnerSL$public_methods$initialize)),
    names(formals(learner$public_methods$initialize))
  )

  # base learners are provided via estimate.args
  lr <- learnerSL$new(estimate.args = list(learners = lrs, nfolds = 2))
  expect_equal(deparse(lr$formula), "y ~ 1")
  expect_equal(names(lr$learners), c("mean", "glm"))
  expect_equal(lr$opt("nfolds"), 2)
  expect_equal(lr$info, "superlearner\n\tmean\n\tglm")
  expect_error(learnerSL$new(), pattern = "non-empty list")
  expect_error(
    learnerSL$new(estimate.args = list(nfolds = 2)),
    pattern = "non-empty list"
  )

  # superlearner and its predict method are the default estimate and predict
  # methods
  lr_sum <- lr$summary()
  expect_identical(lr_sum$estimate, superlearner)
  expect_identical(lr_sum$predict, targeted:::predict.superlearner)

  # defaults of superlearner apply to arguments that are not provided
  lr <- learnerSL$new(estimate.args = list(learners = lrs))
  expect_null(lr$opt("nfolds"))
  lr$estimate(d)
  expect_equal(length(lr$fit$folds), 10)

  # formula argument is not used
  expect_warning(
    learnerSL$new(yb ~ x1, estimate.args = list(learners = lrs)),
    pattern = "'formula' is not used"
  )
  lr <- suppressWarnings(
    learnerSL$new(yb ~ x1, estimate.args = list(learners = lrs))
  )
  expect_equal(deparse(lr$formula), "y ~ 1")
  expect_equal(unname(base_formulas(lr)), c("y ~ 1", "y ~ x1 + x2"))

  expect_warning(
    learnerSL$new(
      estimate.args = list(learners = lrs),
      formula.keep.specials = TRUE
    ),
    pattern = "formula.keep.specials"
  )
  expect_equal(
    learnerSL$new(estimate.args = list(learners = lrs), info = "sl")$info,
    "sl"
  )

  # user-defined estimate method receives the base learners via 'learners'
  lr <- learnerSL$new(
    estimate = function(data, learners, ...) {
      superlearner(
        learners = learners, data = data,
        meta.learner = metalearner_discrete, ...
      )
    },
    estimate.args = list(learners = lrs, nfolds = 2)
  )
  lr$estimate(d)
  expect_true(all(weights(lr$fit) %in% c(0, 1)))
  expect_equal(sum(weights(lr$fit)), 1)

  # predict.args and predict.filter are passed on to learner$new()
  lr <- learner_sl(lrs, nfolds = 2,
    learner.args = list(predict.args = list(all.learners = TRUE))
  )
  lr$estimate(d)
  expect_equal(dim(lr$predict(sim1(5))), c(5, 2))

  lr <- learner_sl(lrs, nfolds = 2,
    learner.args = list(predict.filter = \(data) \(pred, newdata) pmax(pred, 0))
  )
  lr$estimate(d)
  expect_true(all(lr$predict(d) >= 0))
}
test_learner_sl_new()

test_learner_sl_update <- function() {
  lrs <- list(
    glm = learner_glm(y ~ x1 + x2 + cos(x1)),
    mean = learner_glm(y ~ 1)
  )
  lr <- learner_sl(lrs, nfolds = 2)

  # response is updated for super learner and all base learners, where the
  # base learners keep their covariates
  lr$update("yb")
  expect_equal(deparse(lr$formula), "yb ~ x1 + x2 + cos(x1)")
  expect_equal(
    unname(base_formulas(lr)),
    c("yb ~ x1 + x2 + cos(x1)", "yb ~ 1")
  )
  # environment of the formula of the super learner is preserved
  expect_identical(environment(lr$formula), environment(lrs$glm$formula))

  # learners used to create the super learner are not modified
  expect_equal(deparse(lrs$glm$formula), "y ~ x1 + x2 + cos(x1)")

  # response variable defined by a function call
  lr$update("I(yb == 1)")
  expect_equal(deparse(lr$formula), "I(yb == 1) ~ x1 + x2 + cos(x1)")
  expect_equal(
    unname(base_formulas(lr)),
    c("I(yb == 1) ~ x1 + x2 + cos(x1)", "I(yb == 1) ~ 1")
  )

  # warn when providing formula with covariates
  pat <- "only updates the response"
  expect_warning(lr$update(yb ~ x1 + x2 + cos(x1)), pattern = pat)
  expect_warning(lr$update(y ~ .), pattern = pat)
  expect_warning(lr$update("yb ~ cos(x1) + x2 + x1"), pattern = pat)
  expect_equal(
    unname(base_formulas(lr)),
    c("yb ~ x1 + x2 + cos(x1)", "yb ~ 1")
  )

  # require response variable
  expect_error(lr$update(~ x1), pattern = "response variable")

  # estimation uses the updated response
  lr$update("yb")
  lr$estimate(d)
  expect_equal(
    unname(vapply(lr$fit$fit, \(x) deparse(x$formula[[2]]), character(1))),
    c("yb", "yb")
  )

  # variables are looked up correctly in global env
  thr <- 2
  lr$update("I(y < thr)")
  lr$estimate(d)
  expect_equal(deparse(lr$learners[[1]]$formula[[2]]), "I(y < thr)")
}
test_learner_sl_update()

test_learner_sl_clone <- function() {
  lr <- learner_sl(
    list(mean = learner_glm(y ~ 1), glm = learner_glm(y ~ x1 + x2)),
    nfolds = 2
  )
  lr_clone <- lr$clone(deep = TRUE)
  lr_clone$update("yb")

  # updating the clone does not modify the original object
  expect_equal(deparse(lr$formula), "y ~ 1")
  expect_equal(deparse(lr_clone$formula), "yb ~ 1")
  expect_equal(unname(base_formulas(lr)), c("y ~ 1", "y ~ x1 + x2"))
  expect_equal(unname(base_formulas(lr_clone)), c("yb ~ 1", "yb ~ x1 + x2"))

  # each object is estimated with its own base learners
  resp <- function(sl) {
    unname(vapply(sl$fit$fit, \(x) deparse(x$formula[[2]]), character(1)))
  }
  lr_clone$estimate(d)
  lr$estimate(d)
  expect_equal(resp(lr), c("y", "y"))
  expect_equal(resp(lr_clone), c("yb", "yb"))
}
test_learner_sl_clone()

test_learner_sl_cate <- function() {
  # cate updates the response of the treatment model with I(a == level) for
  # each treatment level. The base learners must be updated accordingly, such
  # that a super learner with a single base learner gives the same estimates
  # as the base learner itself.
  set.seed(1)
  n <- 500
  x <- rnorm(n)
  a <- rbinom(n, 1, lava::expit(x))
  y <- a + x + rnorm(n)
  dd <- data.frame(y, a, x)

  tm <- learner_sl(list(glm = learner_glm(a ~ x, family = binomial)))

  fit_sl <- cate(
    treatment.model = tm,
    response.model = learner_glm(y ~ a + x),
    cate.model = ~1, data = dd
  )


  fit_glm <- cate(
    treatment.model = learner_glm(a ~ x, family = binomial),
    response.model = learner_glm(y ~ a + x),
    cate.model = ~1, data = dd
  )
  expect_equal(coef(fit_sl), coef(fit_glm))

  # treatment model provided by the user is not modified
  expect_equal(deparse(tm$formula), "a ~ x")
  expect_equal(unname(base_formulas(tm)), "a ~ x")
}
test_learner_sl_cate()
