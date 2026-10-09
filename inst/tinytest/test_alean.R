library("tinytest")

set.seed(42)
n <- 500
x <- rnorm(n)
a <- sample(0:2, n, replace = TRUE) # > 2 levels: g_model is estimated
y <- a + x + x^2 + rnorm(n)
d <- data.frame(y = y, a = a, x = x)

fit_alean <- function(...) {
  set.seed(1) # folds are random
  alean(
    response_model = learner_glm(y ~ a + x),
    exposure_model = learner_glm(a ~ x),
    data = d, nfolds = 2, silent = TRUE, ...
  )
}

test_alean_g_model <- function() {
  # alean() replaces the response variable of g_model. Models with and without
  # response variable are therefore the same model
  expect_equal(
    coef(fit_alean(g_model = learner_glm(y ~ x + I(x^2)))),
    coef(fit_alean(g_model = learner_glm(~ x + I(x^2))))
  )
  # default g_model: response_model without the exposure
  expect_equal(coef(fit_alean()), coef(fit_alean(g_model = learner_glm(~ x))))
}
test_alean_g_model()

test_alean_g_model_environment <- function() {
  # variables of the g_model formula that are not in the data are found in
  # the environment of the formula, also after the response variable of
  # g_model is replaced
  g_model <- function() {
    poly_degree <- 2 # not a column of d
    learner_glm(y ~ poly(x, degree = poly_degree))
  }
  expect_equal(
    coef(fit_alean(g_model = g_model())),
    coef(fit_alean(g_model = learner_glm(y ~ poly(x, degree = 2))))
  )
}
test_alean_g_model_environment()
