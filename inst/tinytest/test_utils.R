library("tinytest")

test_softmax <- function() {
  X <- cbind(1, 1)
  expect_equivalent(softmax(X, ref = FALSE), cbind(.5, .5))

  X <- rnorm(10)
  expect_equivalent(softmax(X), lava::expit(X))

  X <- cbind(1, 2, 3)
  expect_equivalent(softmax(X), exp(cbind(0, X)) / sum(exp(c(0, X))))
}
test_softmax()

test_nondom <- function() {
  x <- rbind(
    c(1.0, 0.5),
    c(0.0, 1.0),
    c(1.0, 0.0),
    c(0.5, 1.0),
    c(1.0, 1.0),
    c(0.8, 0.8)
  )
  y <- nondom(x)

  res <- apply(y, 1, identity, simplify = FALSE)
  true <- list(c(0, 1), c(0.8, 0.8), c(1, 0.5))
  expect_true(nrow(y) == 3)
  expect_true(length(setdiff(res, true)) == 0)
}
test_nondom()
