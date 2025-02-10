library(tinytest)

n <- 1e3
ddata <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
ddata$y <- with(ddata, x1 * 2 - x2 + rnorm(n))

# test various ways to initialize an ml_model
test_initialize <- function() {
  # formula supplied + used in estimate function
  m1 <- ml_model$new(
    formula = y ~ -1 + x1 + x2,
    estimate = glm
  )
  m1$estimate(ddata)

  # formula supplied + not used in estimate function -> use targeted::design
  m2 <- ml_model$new(
    formula = y ~ x1 + x2,
    estimate = glm.fit,
    predict = \(object, newdata) newdata %*% object$coefficients
  )
  m2$estimate(ddata)

  expect_equal(m1$predict(newdata = ddata), m2$predict(newdata = ddata)[, 1])

  # no formula supplied but passed when estimating model
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
    formula = y ~ -1 + x1 + x2,
    estimate = glm,
    weights = ww
  )
  m1_weights$estimate(ddata)
  expect_equal(coef(m1_weights$fit), coef(fit))
  # additional check to verify that weights change the estimates
  expect_false(all(coef(m1_weights$fit) == coef(m1$fit)))
}
test_initialize()
