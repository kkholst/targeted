sim1 <- function(n = 5e2) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
   d <- data.frame(y, x1, x2)
   d
}
d0 <- sim1()

test_superlearner <- function() {
  lrs <- list(mean = predictor_glm(y ~ 1), glm = predictor_glm(y ~ x1))
  sl <- superlearner(lrs, data = d0, nfolds = 2)
  
}
