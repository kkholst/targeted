context("cumres")

## Cumulative residuals test

testthat::test_that("One predictor, small sample size", {
    n <- 500
    x <- runif(n)
    y <- x + rnorm(n)
    l <- lm(y ~ x)
    g <- cumres(l)
    cc <- coef(g)
    testthat::expect_equal(dim(cc), c(2,2))
    testthat::expect_true(!any(is.na(cc)))
})


testthat::test_that("Several predictor, large sample size", {
    n <- 1e4
    p <- 10
    X <- matrix(rnorm(p*n), ncol=p)
    y <- X %*% rep(1,p) + rnorm(n)
    l <- lm(y ~ X)
    g <- cumres(l)
    cc <- coef(g)
    testthat::expect_equal(dim(cc), c(p+1, 2))
    testthat::expect_true(!any(is.na(cc)))

    g0 <- cumres(l, subset=NULL)
    cc0 <- coef(g0)
    testthat::expect_equal(dim(cc0), c(p+1, 2))
    testthat::expect_true(!any(is.na(cc0)))

    r <- residuals(l)
    yhat <- predict(l)
    ord <- order(yhat)
    W <- 1/sqrt(n)*cumsum(r[ord])
    testthat::expect_equivalent(W,g$W[[1]])
})
