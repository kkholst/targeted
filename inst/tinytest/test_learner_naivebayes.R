set.seed(42)

sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   return(data.frame(y, yb, x1, x2))
}
d <- sim1(1e3)

# binary classification
lr <- learner_naivebayes(yb ~ x1 + x2)
lr$estimate(d)

pr <- lr$predict(d)
expect_true(all(pr > 0 & pr < 1))
expect_null(ncol(pr)) # return vector of class 2 probabilities for binary class

# verify that arguments are passed on to NB
lr <- learner_naivebayes(yb ~ x1 + x2, kernel = TRUE, laplace.smooth = 1)
lr$estimate(d)
pr1 <- lr$predict(d)
expect_true(all(pr != pr1))
expect_true(lr$fit$call$kernel)
expect_equal(lr$fit$call$laplace.smooth, 1)

# arguments can be overwritten in estimate method call
lr$estimate(d, kernel = FALSE)
pr2 <- lr$predict(d)
expect_true(all(pr2 != pr1))
expect_false(lr$fit$call$kernel)

# support for factor transformation of response variable
lr <- learner_naivebayes(factor(yb) ~ x1 + x2)
lr$estimate(d)

# comparison with e1071 implementation
fit <- e1071::naiveBayes(factor(yb) ~x1+x2, d, laplace = 0)
pr <- lr$predict(d)
pr2 <- predict(fit, d, type="raw")[,2]
expect_true(mean((pr-pr2)**2)<1e-6)

# multi-class classification
lr <- learner_naivebayes(Species ~ .)
lr$estimate(iris)
expect_equal(dim(lr$predict(head(iris))), c(6, 3))

# frequency weights
d$x <- factor(d$x1>0)
lr <- learner_naivebayes(yb ~ x)
lr$estimate(d)
dd <- data.table(d)[,.(.N),by=.(yb,x)]
lr2 <- learner_naivebayes(yb ~ x + weights(N))
lr2$estimate(dd)
expect_equal(lr$predict(dd), lr2$predict(dd))
