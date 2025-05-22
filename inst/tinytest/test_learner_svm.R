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
