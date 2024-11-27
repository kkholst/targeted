n <- 10
x <- runif(n, -4, 4)
y <- rbinom(n, 1, lava::expit(-1 + x))
ord <- order(x)
pv <- targeted::pava(y[ord])
xx <- x[ord[pv$index]]
yy <- pv$value
af <- stepfun(c(xx), c(yy, max(yy)), f = 0, right = TRUE)
af2 <- as.stepfun(stats::isoreg(x, y))
expect_equal(sum((af(x) - af2(x))^2), 0, tol = 1e-1)
