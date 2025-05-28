library(tinytest)

n <- 1e3
ddata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
ddata$y <- with(ddata, x1 * 2 - x2 + rnorm(n))

ddata_count <- data.frame(x = rnorm(n), w = rep(c(1, 2), length.out = n))
ddata_count$y <- with(ddata_count, rpois(n, exp(2 + 0.5 * x) * w))

# test various ways to initialize an learner object
test_initialize <- function() {
  # formula supplied + used in estimate function
  m1 <- learner$new(formula = y ~ -1 + x1 + x2, estimate = glm)
  m1$estimate(ddata)

  # formula supplied + not used in estimate function -> use targeted::design
  m2 <- learner$new(
    formula = y ~ x1 + x2,
    estimate = glm.fit,
    predict = \(object, newdata) newdata %*% object$coefficients
  )
  m2$estimate(ddata)

  expect_equal(m1$predict(newdata = ddata), m2$predict(newdata = ddata)[, 1])

  # no formula supplied but passed when estimating models
  m3 <- learner$new(
    estimate = \(formula, data) glm(formula, data = data)
  )
  m3$estimate(data = ddata, formula = y ~ -1 + x1 + x2)

  expect_equal(m1$predict(newdata = ddata), m3$predict(newdata = ddata))

  # test response.arg and x.arg work as expected
  m4 <- learner$new(
    formula = y ~ x1 + x2,
    estimate = \(yy, xx) glm.fit(y = yy, x = xx),
    predict = \(object, newdata) newdata %*% object$coefficients,
    response.arg = "yy",
    x.arg = "xx"
  )
  m4$estimate(ddata)
  expect_equal(m1$predict(newdata = ddata), m4$predict(newdata = ddata)[, 1])

  # test that optional arguments are passed on to fitting function
  ww <- rep(c(0, 1), length.out = n)
  fit <- glm(y ~ -1 + x1 + x2, data = ddata, weights = ww)

  m1_weights <- learner$new(
    formula = y ~ -1 + x1 + x2, estimate = glm,
    estimate.args = list(weights = ww)
  )
  m1_weights$estimate(ddata)
  expect_equal(coef(m1_weights$fit), coef(fit))
  # additional check to verify that weights change the estimates
  expect_false(all(coef(m1_weights$fit) == coef(m1$fit)))

  # test that predict.args are passed on correctly to predict function
  m_count <- learner$new(
    formula = y ~ x + offset(w), estimate = glm,
    estimate.args = list(family = poisson),
    predict.args = list(type = "response")
  )
  m_count$estimate(ddata_count)

  fit_count <- glm(y ~ x + offset(w), family = poisson, data = ddata_count)
  expect_equal(
    m_count$predict(ddata_count),
    predict(fit_count, ddata_count, type = "response")
  )
}
test_initialize()

# test estimation method
test_estimate <- function() {
  # verify that optional arguments are passed on to fitfun
  ww <- rep(c(0, 1), length.out = n)
  m1 <- learner$new(formula = y ~ x1 + x2, estimate = glm)
  fit_ml <- m1$estimate(ddata, weights = ww)

  fit_glm <- glm(y ~ x1 + x2, data = ddata, weights = ww)
  expect_equal(coef(m1$fit), coef(fit_glm))
  # verify that estimate method returns output of estimate function
  expect_equal(coef(fit_ml), coef(fit_glm))

  # arguments to fitfun when supplied during initialization can be overriden
  m2 <- learner$new(formula = y ~ x1 + x2, estimate = glm,
    estimate.args = list(weights = rep(1, n))
  )
  m2$estimate(data = ddata, weights = ww)
  expect_equal(coef(m2$fit), coef(fit_glm))

  # test estimate function which takes x, y as arguments
  m3 <- learner$new(formula = y ~ x1 + x2, estimate = glm.fit,
    intercept = TRUE, estimate.args = list(weights = ww)
  ) # intercept = TRUE is required because no intercept is added when deriving
  # the design matrix from the provided formula argument
  m3$estimate(ddata)
  expect_equal(coef(m3$fit), coef(fit_glm))

  # verify that arguments are passed on to fitfun
  m3$estimate(data = ddata, weights = rep(1, n))
  expect_true(all(coef(m3$fit) != coef(fit_glm)))

  # specials are correctly handled for estimating method with x, y arguments
  m4 <- learner$new(formula = y ~ x + offset(log(w)), estimate = glm.fit,
    intercept = TRUE, specials = "offset",
    estimate.args = list(family = poisson())
  )
  m4$estimate(ddata_count)
  fit <- glm(y ~ x + offset(log(w)), data = ddata_count, family = poisson())
  expect_equal(coef(fit), coef(m4$fit))

  # it is currently not possible for the estimate method call to pass arguments
  # to targeted::design inside the fitfun
  m4 <- learner$new(formula = y ~ x + offset(log(w)), estimate = glm.fit,
    intercept = TRUE, estimate.args = list(family = poisson())
  )
  m4$estimate(ddata_count, specials = c("offset"))
  expect_true(all(coef(fit) != coef(m4$fit)))

  # ml model can also be used with a formula argument. verify that family
  # argument is passed correctly on to fitfun upon initialization + offset
  # during method call
  m4 <- learner$new(
    estimate = glm.fit, estimate.args = list(family = poisson())
  )
  .design <- design(y ~ x + offset(log(w)), ddata_count, intercept = TRUE)
  m4$estimate(.design$x, .design$y, offset = .design$offset)
  expect_equal(coef(fit), coef(m4$fit))
}
test_estimate()

# test predict method
test_predict <- function() {
  fit_glm <- glm(y ~ x + offset(w), family = poisson, data = ddata_count)

  m <- learner$new(
    formula = y ~ x + offset(w), estimate = glm,
    estimate.args = list(family = poisson)
  )
  fit_ml <- m$estimate(ddata_count)

  expect_equal(m$predict(ddata_count), predict(fit_glm, ddata_count))
  # method also works as expect when supplying fitted object
  expect_equal(
    m$predict(ddata_count, object = fit_ml),
    predict(fit_glm, ddata_count)
  )
  # method passes on additional arguments to underlying predict function
  expect_equal(
    m$predict(ddata_count, type = "response"),
    predict(fit_glm, ddata_count, type = "response")
  )

  # error when trying to override predict.args during predict method call
  m1 <- learner$new(
    formula = y ~ x + offset(w), estimate = glm,
    estimate.args = list(family = poisson),
    predict.args = list(type = "link")
  )
  m1$estimate(ddata_count)
  expect_equal(
    m1$predict(ddata_count, type = "response"),
    predict(fit_glm, ddata_count, type = "response")
  )
}
test_predict()

test_design <- function() {
  m <- learner$new(formula = y ~ x1 + x2, estimate = glm.fit, intercept = TRUE)
  m$estimate(ddata)

  # options defined for targeted::design upon ml model initialization are passed
  # on to method call
  fit <- glm.fit(x = m$design(ddata)$x, y = m$response(ddata))
  expect_equal(coef(m$fit), coef(fit))

  # test support for . on RHS of formula
  lr <- learner$new(formula = y ~ ., estimate = glm.fit, intercept = TRUE)
  expect_equal(m$design(ddata)$x, lr$design(ddata)$x)

  # remove some features
  lr <- learner$new(formula = y ~ . -x1, estimate = glm.fit, intercept = FALSE)
  expect_equal(colnames(lr$design(ddata)$x), "x2")

  # works with offset
  lr <- learner$new(formula = y ~ . -x1 + offset(x1), estimate = glm.fit,
    specials = c("offset"))
  expect_equivalent(lr$design(ddata)$offset, ddata$x1)

  # defined options can be overruled during method call
  fit <- glm.fit(x = m$design(ddata, intercept = FALSE)$x, y = m$response(ddata))
  expect_false("(Intercept)" %in% names(coef(fit)))

  # raise error when covariate variable cannot be found
  m <- learner$new(formula = y ~ x1 + x2 + x3, estimate = glm.fit)
  expect_error(m$design(ddata), pattern = "object 'x3' not found")
  # same for response
  m <- learner$new(formula = yy ~ x1 + x2, estimate = glm.fit)
  expect_error(m$design(ddata), pattern = "object 'yy' not found")

}
test_design()

test_update <- function() {
  lr <- learner$new(formula = y ~ -1 + x1 + x2, estimate = glm)
  lr$update(y ~ x1)
  lr$estimate(ddata)

  fit_ref <- glm(y ~ x1, data = ddata)
  expect_equal(coef(lr$fit), coef(fit_ref))

  # also supports character arguments
  lr$update("y ~ x1 + x2")
  lr$estimate(ddata)
  fit_ref <- glm(y ~ x1 + x2, data = ddata)
  expect_equal(coef(lr$fit), coef(fit_ref))
}
test_update()

test_response <- function() {
  lr <- learner$new(formula = I(y > 0) ~ -1 + x1 + x2, estimate = glm)

  expect_equivalent(lr$response(ddata), ddata$y > 0)
  expect_equivalent(lr$response(ddata, eval = FALSE), ddata$y)

  # different response name then y
  lr <- learner$new(formula = x1 ~ -1 + y, estimate = glm)
  expect_equivalent(lr$response(ddata), ddata$x1)

  # raise error when response variable cannot be found
  lr <- learner$new(formula = x3 ~ -1 + y, estimate = glm)
  expect_error(lr$response(ddata), pattern = "object 'x3' not found")
}
test_response()

test_summary <- function() {
  lr <- learner_glm(y ~ x1 + x2, family = "nb")
  lr_sum <- lr$summary()
  expect_stdout(print(lr_sum), "formula: y \\~ x1 \\+ x2")
  expect_stdout(print(lr_sum), "estimate: formula, data, family, ...")
  expect_stdout(print(lr_sum), "estimate.args: family=nb")
  expect_stdout(print(lr_sum), "predict: object, newdata, ... ")
  expect_stdout(print(lr_sum), "predict.args: ")
  expect_stdout(print(lr_sum), "specials: ")

  # simple checks that relevant keys are populated in the returned list
  expect_equal(lr_sum$info, "glm")
  expect_equal(lr_sum$intercept, FALSE)
  expect_equal(lr_sum$x.arg, "x")
  expect_equal(lr_sum$response.arg, "y")

  # verify that updated response is printed correctly
  lr$update(y ~ x)
  expect_stdout(print(lr$summary()), "formula\\: y \\~ x")
  # same for info
  lr$info <- "new info"
  expect_stdout(print(lr$summary()), "new info")

  # verify that console is not cluttered for learner_sl by printing detailed
  # information about all learners
  lr <- learner_sl(list(learner_glm(y ~ x), learner_gam(y ~ s(x))), nfolds = 5)
  lr_sum <- lr$summary()
  expect_stdout(
    print(lr_sum),
    pattern = paste0(
      "estimate.args: learners=<list>, nfolds=5, meta.learner=<function>, ",
      "model.score=<function>"
    ))
}
test_summary()

test_ml_model <- function() {
  lr <- learner$new(formula = y ~ -1 + x1 + x2, estimate = glm)
  expect_warning(
    ml <- ml_model$new(formula = y ~ -1 + x1 + x2, estimate = glm),
    pattern = "targeted::ml_model is deprecated"
  )
  lr$estimate(ddata)
  ml$estimate(ddata)
  expect_equal(coef(lr$fit), coef(ml$fit))
}
test_ml_model()
