set.seed(42)

sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   return(data.frame(y, yb, x1, x2))
}
d <- sim1()


# classification / binary prediction
# transform outcome internally to factor for probability = TRUE
set.seed(42)
lr <- learner_svm(yb ~ x1 * x2, probability = TRUE)
lr$estimate(d)

set.seed(42)
lr_fact <- learner_svm(as.factor(yb) ~ x1 * x2, probability = TRUE)
lr_fact$estimate(d)

expect_equal(lr$predict(d), lr_fact$predict(d))
# predictions are a vector for binary classification
expect_null(ncol(lr$predict(d)))
# predict classes
expect_true(is.factor(lr$predict(d, probability = FALSE)))


# don't transform outcome when probability = FALSE -> regression problem
lr <- learner_svm(yb ~ x1 * x2, probability = FALSE)
lr$estimate(d)
expect_true(any(lr$predict(d) > 1))
# predictions are a vector for regression problems
expect_null(ncol(lr$predict(d)))

# parameters are passed on correctly to estimate function
lr <- learner_svm(yb ~ ., cost = 0.5, epsilon = 0.5, kernel = "linear")
lr$estimate(d)
expect_equal(lr$fit$epsilon, 0.5)
expect_equal(lr$fit$cost, 0.5)
expect_equal(lr$fit$call$kernel, "linear")

# estimate.args can be overruled in estimate method call
lr$estimate(d, cost = 2)
expect_equal(lr$fit$cost, 2)

# multi-class classification
lr <- learner_svm(Species ~ ., probability = TRUE)
lr$estimate(iris)
lr$predict(head(iris))
