set.seed(42)

sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   return(data.frame(y, yb, x1, x2))
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
