library("tinytest")

n <- 1000
x <- rnorm(n)
a <- rbinom(n, 1, expit(1 + x))
y <- 1 + a + x - a * x + rnorm(n)
d <- data.frame(y = y, a = a, x = x)

true_estimate1 <- function(q1) {
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
  return(estimate(coef = est, IC = ic))
}

simcate <- function(qmod) {
  q1 <- predict(lm(qmod, data = d),
    newdata = transform(d, a = 1)
  )
  e1 <- true_estimate1(q1)
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


test_cate_deprecated_arguments <- function() {
  qmod <- y ~ a * x
  q1 <- predict(lm(qmod, data = d), newdata = transform(d, a = 1))
  e1 <- true_estimate1(q1)
  # use deprecated argument names to verify that cate continues to work as
  # expected
  expect_warning(
    aa1 <- cate(
      response_model = ML(qmod),
      propensity.model = ML(a ~ x, family = binomial),
      data = d
    ) |> estimate(),
    pattern = "Please use the `response.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa1)["E[y(1)]", 1:2])

  expect_warning(
    aa2 <- cate(
      response.model = ML(qmod),
      propensity_model = ML(a ~ x, family = binomial),
      data = d
    ) |> estimate(),
    pattern = "Please use the `propensity.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa2)["E[y(1)]", 1:2])

  # user is informed when deprecated treatment argument is used
  expect_warning(aa4 <- cate(
    response.model = ML(qmod),
    propensity.model = ML(a ~ x, family = binomial),
    treatment = ~ 1,
    data = d) |> estimate(),
    pattern = "Please use the `cate.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa4)["E[y(1)]", 1:2])

  # same with cate_model argument
  expect_warning(aa4 <- cate(
    response.model = ML(qmod),
    propensity.model = ML(a ~ x, family = binomial),
    cate_model = ~ 1,
    data = d) |> estimate(),
    pattern = "Please use the `cate.model` argument instead"
  )
  expect_equivalent(parameter(e1)[1:2], parameter(aa4)["E[y(1)]", 1:2])

  # function fails when old treatment arg and new cate.model arg are used
  # together
  expect_error(
    cate(
      response.model = ML(qmod),
      propensity.model = ML(a ~ x, family = binomial),
      cate.model = ~2,
      treatment = ~1,
      data = d
    ),
    pattern = "Calling `cate` with both the obsolete"
  )
}
test_cate_deprecated_arguments()
