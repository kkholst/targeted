set.seed(42)

n <- 5e2
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
y <-  x1 + rnorm(n, sd = 0.5**.5)
d <- data.frame(y,  x1, x2)

lr <- learner_isoreg(y ~ x1)
lr$estimate(d)
pr <- lr$predict(data.frame(x1 = -100:100))
# verify that predictions are monotonically increasing
expect_true(all(diff(pr) >= 0))

expect_error(
  learner_isoreg(y ~ x1 + x2),
  pattern = "learner_isoreg: expected one outcome and one predictor variable"
)

# permit usage of . in formula but catch error in isoregw that two predictors
# are used
lr <- learner_isoreg(y ~ .)
expect_error(
  lr$estimate(d),
  pattern = "Expect only one predictor variable"
)
