testthat::context("Risk regression")

set.seed(1)
m <- lava::lvm(a[-2] ~ 1*x+z,
               linpred.target[1] ~ 1,
               linpred.nuisance[-1] ~ 2*x+z)
lava::distribution(m,~a) <- lava::binomial.lvm("logit")
m <- lava::binomial.rr(m, "y","a","linpred.target","linpred.nuisance")
d <- lava::sim(m,1e3)

testthat::test_that("MLE convergence", {
    fit <- targeted::riskreg(y ~ a | 1 | x+z | 1, data=d, type="rr")
    influ <- lava::iid(fit)    
    val <- sum(colMeans(influ)^2)
    testthat::expect_true(val<1e-8)
})


