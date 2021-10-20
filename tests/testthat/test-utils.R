testthat::context("Utility functions")

testthat::test_that("softmax", {
  X <- cbind(1, 1)
  testthat::expect_equivalent(softmax(X, ref=FALSE),
                              cbind(.5, .5))
  X <- rnorm(10)
  testthat::expect_equivalent(softmax(X), lava::expit(X))

  X <- cbind(1,2,3)
  testthat::expect_equivalent(softmax(X),
                              exp(c(0,X))/sum(exp(c(0,X))))
})
