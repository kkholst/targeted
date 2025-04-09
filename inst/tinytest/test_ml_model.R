library(tinytest)

n <- 1e3
ddata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
ddata$y <- with(ddata, x1 * 2 - x2 + rnorm(n))

ddata_count <- data.frame(x = rnorm(n), w = rep(c(1, 2), length.out = n))
ddata_count$y <- with(ddata_count, rpois(n, exp(2 + 0.5 * x) * w))

# test various ways to initialize an ml_model
test_initialize <- function() {
  # formula supplied + used in estimate function
  m1 <- ml_model$new(formula = y ~ -1 + x1 + x2, estimate = glm)
  m1$estimate(ddata)

  # formula supplied + not used in estimate function -> use targeted::design
  m2 <- ml_model$new(
    formula = y ~ x1 + x2,
    estimate = glm.fit,
    predict = \(object, newdata) newdata %*% object$coefficients
  )
  m2$estimate(ddata)

  expect_equal(m1$predict(newdata = ddata), m2$predict(newdata = ddata)[, 1])

  # no formula supplied but passed when estimating models
  m3 <- ml_model$new(
    estimate = \(formula, data) glm(formula, data = data)
  )
  m3$estimate(data = ddata, formula = y ~ -1 + x1 + x2)

  expect_equal(m1$predict(newdata = ddata), m3$predict(newdata = ddata))

  # test response.arg and x.arg work as expected
  m4 <- ml_model$new(
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

  m1_weights <- ml_model$new(
    formula = y ~ -1 + x1 + x2, estimate = glm, weights = ww
  )
  m1_weights$estimate(ddata)
  expect_equal(coef(m1_weights$fit), coef(fit))
  # additional check to verify that weights change the estimates
  expect_false(all(coef(m1_weights$fit) == coef(m1$fit)))

  # test that predict.args are passed on correctly to predict function
  m_count <- ml_model$new(
    formula = y ~ x + offset(w), estimate = glm, family = poisson,
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
  m1 <- ml_model$new(formula = y ~ x1 + x2, estimate = glm)
  fit_ml <- m1$estimate(ddata, weights = ww)

  fit_glm <- glm(y ~ x1 + x2, data = ddata, weights = ww)
  expect_equal(coef(m1$fit), coef(fit_glm))
  # verify that estimate method returns output of estimate function
  expect_equal(coef(fit_ml), coef(fit_glm))

  # arguments to fitfun when supplied during initialization can be overriden
  m2 <- ml_model$new(formula = y ~ x1 + x2, estimate = glm, weights = rep(1, n))
  m2$estimate(data = ddata, weights = ww)
  expect_equal(coef(m2$fit), coef(fit_glm))

  # test estimate function which takes x, y as arguments
  m3 <- ml_model$new(formula = y ~ x1 + x2, estimate = glm.fit,
    intercept = TRUE, weights = ww) # intercept = TRUE is required because no
    # intercept is added when deriving the design matrix from the provided
    # formula argument
  m3$estimate(ddata)
  expect_equal(coef(m3$fit), coef(fit_glm))

  # verify that arguments are passed on to fitfun
  m3$estimate(data = ddata, weights = rep(1, n))
  expect_true(all(coef(m3$fit) != coef(fit_glm)))

  # specials are correctly handled for estimating method with x, y arguments
  m4 <- ml_model$new(formula = y ~ x + offset(log(w)), estimate = glm.fit,
    intercept = TRUE, specials = "offset", family = poisson())
  m4$estimate(ddata_count)
  fit <- glm(y ~ x + offset(log(w)), data = ddata_count, family = poisson())
  expect_equal(coef(fit), coef(m4$fit))

  # it is currently not possible for the estimate method call to pass arguments
  # to targeted::design inside the fitfun
  m4 <- ml_model$new(formula = y ~ x + offset(log(w)), estimate = glm.fit,
    intercept = TRUE, family = poisson())
  m4$estimate(ddata_count, specials = c("offset"))
  expect_true(all(coef(fit) != coef(m4$fit)))

}
test_estimate()

# test predict method
test_predict <- function() {
  fit_glm <- glm(y ~ x + offset(w), family = poisson, data = ddata_count)

  m <- ml_model$new(
    formula = y ~ x + offset(w), estimate = glm, family = poisson
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
  m1 <- ml_model$new(
    formula = y ~ x + offset(w), estimate = glm, family = poisson,
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
  m <- ml_model$new(formula = y ~ x1 + x2, estimate = glm.fit, intercept = TRUE)
  m$estimate(ddata)

  # options defined for targeted::design upon ml model initialization are passed
  # on to method call
  fit <- glm.fit(x = m$design(ddata), y = m$response(ddata))
  expect_equal(coef(m$fit), coef(fit))

  # defined options can be overruled during method call
  fit <- glm.fit(x = m$design(ddata, intercept = FALSE), y = m$response(ddata))
  expect_false("(Intercept)" %in% names(coef(fit)))
}
test_design()
