library("tinytest")

n <- 1000
x <- rnorm(n)
a <- rbinom(n, 1, expit(1 + x))
y <- 1 + a + x - a * x + rnorm(n)
d <- data.frame(y = y, a = a, x = x)

simcate <- function(qmod) {
  q1 <- predict(lm(qmod, data = d),
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
  E1 <- colMeans(cbind(D, D * x))
  ic1 <- IC(g) %*% E1
  ic <- ic0 + ic1
  e1 <- estimate(coef = est, IC = ic)

  aa <- cate(
    ML(qmod),
    ML(a ~ x, family = binomial),
    data = d, nfolds = 1
  ) |> estimate()

  expect_equivalent(
    parameter(e1)[1:2],
    parameter(aa)["E[y(1)]", 1:2]
  )
}
simcate(y ~ a * x) # cate: correct q-model
simcate(y ~ a + x) # cate: mis-specified q-model
