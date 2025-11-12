set.seed(42)

simcount <- function(n = 5e2) {
  x <- rnorm(n)
  w <- 50 + rexp(n, rate = 1 / 5)
  y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
  return(data.frame(y, x, w))
}
dcount <- simcount()


test_learner_hal <- function() {
  # verify that family argument is passed on + optional arguments can be
  # passed on to hal9001::fit_hal in learner$estimate call
  lr <- learner_hal(y ~ x + offset(log(w)), family = "poisson")
  lr$estimate(dcount, lambda = c(1e4, 3.5))

  expect_equal(lr$fit$lambda_star, 3.5) # lambda = 1e4 is a over-regularized
  # model

  # verify that offset is handled correctly and that predictions are generated
  # on response scale
  pr <- lr$predict(newdata = data.frame(x = 1, w = c(1, 5)))
  expect_equal(pr[1] * 5, pr[2])

  # optional arguments are passed on to underlying predict function
  pr_link <- lr$predict(newdata = data.frame(x = 1, w = 1), type = "link")
  expect_equivalent(pr[1], exp(pr_link))
}
test_learner_hal()
