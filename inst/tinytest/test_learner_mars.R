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

simcount <- function(n = 5e2) {
  x <- rnorm(n)
  w <- 50 + rexp(n, rate = 1 / 5)
  y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
  return(data.frame(y, x, w))
}
dcount <- simcount()


test_learner_mars <- function() {
  fit_ref <- earth::earth(y ~ x1 + x2, degree = 2, data = d, nfold = 3)
  lr <- learner_mars(y ~ x1 + x2, degree = 2, nfold = 3)
  lr$estimate(d)

  expect_equal(coef(fit_ref), coef(lr$fit))
  expect_equivalent(lr$predict(d), predict(fit_ref, d)[, 1])
  # verifies that optional arguments are passed on via ... to earth::earth
  expect_equal(lr$fit$call$nfold, 3)

  # verifies glm argument + offset
  lr <- learner_mars(y ~ x + offset(log(w)), degree = 2,
    glm = list(family = poisson())
  )
  lr$estimate(dcount)

  # offset is handled correctly when making predictions
  pr <- lr$predict(data.frame(x = 1, w = c(1, 5)))
  expect_equal(pr[1] * 5, pr[2])

  pr_link <- lr$predict(data.frame(x = 1, w = c(1, 5)), type = "link")
  expect_equal(pr, exp(pr_link))
}
test_learner_mars()
