simdata <- function(n=1000) {
  a <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(-1 + a + a * x))
  data.frame(y, a, x)
}
d <- simdata()

test_learner_stratify <- function() {
  lr <- learner_stratify(
    y ~ x + stratify(a),
    learner_glm,
    family=binomial()
  )
  lr$estimate(d)
  pr <- lr$predict(d)

  d0 <- subset(d, a==0)
  d1 <- subset(d, a==1)
  g0 <- glm(y ~ x, family=binomial, data=d0)
  g1 <- glm(y ~ x, family=binomial, data=d1)
  pr0 <- predict(g0, newdata=d, type="response")
  pr1 <- predict(g1, newdata=d, type="response")
  pr. <- with(d, a * pr1 + (1-a) * pr0)

  expect_true(sum(abs(pr-pr.)) == 0)

  # works with only single level in 'a'
  lr$estimate(d0)
  pr <- lr$predict(d0)
  pr0 <- predict(g0, newdata=d0, type="response")
  expect_true(sum(abs(pr-pr0)) == 0)

  # NA when strata was not seen in estimation data
  pr <- lr$predict(d)
  expect_equal(is.na(pr), d$a==1)
}
test_learner_stratify()

# test that learner_stratify works with specials in formulas
test_learner_stratify_specials <- function() {
  m <- learner_expand_grid(
    learner_glm,
    list(
      formula = y ~ offset(x) + a:x,
      family=c("gaussian", "gaussian")
    ),
    names = "GLM"
  )

  lapply(m, \(x) x$estimate(d))
  g <- glm(y ~ offset(x) + a:x, data=d)
  expect_equal(coef(g), coef(m[[1]]$fit))
  expect_equal(coef(g), coef(m[[2]]$fit))
}
test_learner_stratify_specials()
