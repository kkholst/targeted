context("CATE")

n <- 1000
x <- rnorm(n)
a <- rbinom(n, 1, expit(1 + x))
y <- 1 + a + x -a*x + rnorm(n)
d <- data.frame(y = y, a = a, x = x)

testthat::test_that("cate: correct q-model", {
  q1 <- predict(lm(y ~ a*x, data = d),
    newdata = transform(d, a = 1)
    )
  g <- glm(a ~ x, data = d, family = binomial)
  pi <- predict(g, type = "response")
  ic0 <- with(d, a / pi * (y - q1) + q1)
  est <- mean(ic0)
  ic0 <- ic0 - est
  dlinkinv <- g$family$mu.eta
  b <- a / pi^2 * (q1 - y)
  D <- dlinkinv(logit(pi)) * b
  E1 <- colMeans(cbind(D, D*x))
  ic1 <- IC(g) %*% E1
  ic <- ic0 + ic1
  e1 <- estimate(coef = est, IC = ic)

  aa <- cate(a ~ 1,
    ML(y ~ a*x),
    ML(a ~ x, family = binomial),
    data = d, nfolds = 1
  ) |> estimate()

  expect_equivalent(
    parameter(e1)[1:2],
    parameter(aa)["E[y(1)]", 1:2]
  )
})


testthat::test_that("cate: misspecified q-model", {
  q1 <- predict(lm(y ~ a, data = d),
    newdata = transform(d, a = 1)
    )
  g <- glm(a ~ x, data = d, family = binomial)
  pi <- predict(g, type = "response")
  ic0 <- with(d, a / pi * (y - q1) + q1)
  est <- mean(ic0)
  ic0 <- ic0 - est
  dlinkinv <- g$family$mu.eta
  b <- a / pi^2 * (q1 - y)
  D <- dlinkinv(logit(pi)) * b
  E1 <- colMeans(cbind(D, D*x))
  ic1 <- IC(g) %*% E1
  ic <- ic0 + ic1
  e1 <- estimate(coef = est, IC = ic)

  aa <- cate(a ~ 1,
    ML(y ~ a),
    ML(a ~ x, family = binomial),
    data = d, nfolds = 1
  ) |> estimate()

  expect_equivalent(
    parameter(e1)[1:2],
    parameter(aa)["E[y(1)]", 1:2]
  )
})
