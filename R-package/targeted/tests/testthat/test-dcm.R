context("Discrete choice models")

## testthat::test_that("multinomial logistic I", {
    n <- 100
    K <- 20
    x <- cbind(1,matrix(rnorm(K*n, mean=2), ncol=K))
    beta <- numeric(K+1)
    beta[1:min(K,5)] <- c(-1,1,0,0,0)[1:min(K,5)]
    lp <- x%*%beta
    pr <- lava::expit(lp)
    y <- rbinom(n,1,pr)
    d <- data.frame(y=y, x=x[,-1], y0=letters[y+1])
    colnames(d) <- c("y",paste0("x",1:K), "y0")

    kk <- K
    f <- as.formula(paste0("y0~",paste0(paste0("x",1:kk),collapse="+")))

system.time(g0 <- condlogit(f, data=d, basealt=1))

##     system.time(g1 <- condlogit(alt=d$y0, x=x))
##     system.time(g <- glm(y ~ -1+x, family=binomial))

##     testthat::expect_equivalent(logLik(g), logLik(g0))
##     testthat::expect_equivalent(logLik(g1), logLik(g0))

##     pr0 <- predict(g0, newdata=d)
##     pr1 <- predict(g1, x=x)
##     pr <- predict(g,newdata=d,type="response")
##     testthat::expect_equivalent(pr0,pr1)

##     pr0 <- predict(g0, newdata=d, wide=TRUE)
##     testthat::expect_equivalent(pr0[,"b"], pr)
## })


## testthat::test_that("multinomial logistic II", {
##     data(iris)
##     g <- condlogit(Species ~ Sepal.Width, data=iris)
##     pr <- predict(g, newdata=iris, wide=TRUE)
##     cl <- colnames(pr)[apply(pr, 1, which.max)]
##     table(iris$Species, cl)

##     g2 <- nnet::multinom(Species ~ Sepal.Width, data=iris, maxit=1000)
##     pr2 <- predict(g2, newdata=iris, type="probs")

##     testthat::expect_true(sum((pr-pr2)^2)<1e-6)
## })
