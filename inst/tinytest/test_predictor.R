sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   d <- data.frame(y, yb, x1, x2)
   d
}
d <- sim1()


# testing the MARS wrapper
test_predictor_mars <- function() {
  m1 <- predictor_mars(y ~ x1 + x2, degree = 1)
  m2 <- predictor_mars(y ~ x1 + x2, degree = 2)

  # Fit models
  m1$estimate(d)
  m2$estimate(d)

  # In-sample model score
  s1 <- scoring(object = m1, newdata = d)[,"mse"]
  s2 <- scoring(object = m2, newdata = d)[,"mse"]

  expect_true(s2 < s1)
}
test_predictor_mars()


# testing the SVR module
test_predictor_svm <- function() {
  # classification / binary prediction
  m1 <- predictor_svm(yb ~ x1 + x2,
                      cost = 1, epsilon = 0.1,
                      probability = TRUE)
  # Fit model
  m1$estimate(d)

  # In-sample predictions
  pr <- m1$predict(d)
  # Check these are actual probabilities
  expect_true(all(pr >= 0 & pr <= 1))

  m2 <- predictor_svm(yb ~ x1 + x2,
                      kernel = "linear",
                      cost = 0.1, epsilon = 1,
                      probability = TRUE)
  m2$estimate(d)
  pr2 <- m2$predict(d)
  # Check we get different results with different hyper-parameters
  expect_true(mean((pr-pr2)^2) > 1e-2)
}
test_predictor_svm()
