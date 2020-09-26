context("Isotonic regression")

testthat::test_that("Pooled Adjacent Violator Algorithm", {
    onerun <- function() {
        n <- 10
        x <- runif(n,-4,4)
        y <- rbinom(n,1,lava::expit(-1+x))
        ord <- order(x)
        pv <- targeted::pava(y[ord])
        xx <- x[ord[pv$index]]
        yy <- pv$value
        af <- stepfun(c(xx), c(yy,max(yy)), f=0, right=TRUE)
        af2 <- as.stepfun(stats::isoreg(x,y))
        plot(af); lines(af2, col=lava::Col("red",0.2),lwd=5)
        testthat::expect_true(sum((af(x)-af2(x))^2)<1e-12)
    }
    replicate(10,onerun())
})
