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

simcount <- function(n = 5e3) {

}


test_predictor_glm <- function() {
  # basic check that default arguments for predictor_glm perform linear
  # regression
  fit1 <- glm(y ~ x1, data = d)
  lr <- predictor_glm(y ~ x1)
  lr$estimate(d)

  expect_equal(coef(lr$fit), coef(fit1))

  # test offset
  # test predictions on link scale
}
test_predictor_glm()


