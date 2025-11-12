set.seed(42)

sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   a <- rbinom(n, 1, 0.5)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   return(data.frame(y, yb, x1, x2, a))
}
d <- sim1()

params <- list(
  num.trees = 20,
  min.node.size = 4,
  alpha = 0.04,
  sample.fraction = 0.4
)
args <- c(list(formula = y ~ x1 + x2), params)
lr <- do.call(learner_grf, args)
lr$estimate(d)

# verify that parameters are passed on correctly
expect_equal(lr$fit$`_num_trees`, params$num.trees)
expect_equal(
  lr$fit$tunable.params[c("alpha", "sample.fraction", "min.node.size")],
  params[c("alpha", "sample.fraction", "min.node.size")]
)

# additional parameters are passed on correctly via ellipsis argument
lr <- do.call(learner_grf, c(args, list(imbalance.penalty = 1)))
lr$estimate(d)
expect_equal(lr$fit$tunable.params$imbalance.penalty, 1)

# can be overruled in method call
lr$estimate(d, alpha = 0.1, imbalance.penalty = 0)
expect_equal(lr$fit$tunable.params$imbalance.penalty, 0)
expect_equal(lr$fit$tunable.params$alpha, 0.1)

# simple test for predict method
pr <- lr$predict(data.frame(x1 = c(1, 2), x2 = 0))
expect_equal(length(pr), 2)
expect_true(is.vector(pr))

# binary classification
lr <- learner_grf(as.factor(yb) ~ x1 + x2, model = "probability_forest")
lr$estimate(d)
pr <- lr$predict(d)
expect_true(all(pr > 0 & pr < 1))
expect_equal(NCOL(pr), 1)

# multi-class classification
lr <- learner_grf(Species ~ ., model = "probability_forest")
lr$estimate(iris)
pr <- lr$predict(head(iris))
expect_equal(dim(pr), c(6, 3))

# outcome with attributes set
attributes(d$y)$label <- "y"
lr <- learner_grf(y ~ a)
lr$estimate(d)
