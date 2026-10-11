linear_predictor2 <-  function(a, w1, w2) {
  exp(- (w1 - 1)**2 - (w2 - 1)**2) -
    1 * exp(- (w1 + 1)**2 - (w2 + 1)**2) * a
}

linear_predictor1 <- function(a, w1, w2) {
  -1 + w1 + 1.5 * w2 + 0.7 * (w1 - 1)^2 + -0.12 * a +  -1 * a * (w1 + w2)
}

missing_model1 <- function(a, w1, w2) lava::expit(1 + a - w1)

sim_cate <- function(n,
                    linear.predictor = linear_predictor1,
                    nonmissing.probability = missing_model1,
                    randomization.probability = 2 / 3,
                    residual.variance = 1,
                    ...) {
   w1 <- rnorm(n) # covariates
   w2 <- rnorm(n) # ...
   a <- rbinom(n, 1, randomization.probability) # treatment indicator
   lp <- linear.predictor(a = a, w1 = w1, w2 = w2)
   y0 <- lp + rnorm(n, sd=residual.variance) # continuous endpoint
   u0 <- rbinom(n, 1, lava::expit(lp))  # binary endpoint
   r <- rbinom(n, 1, nonmissing.probability(a = a, w1 = w1, w2 = w2))
   y <- y0 # endpoint with missing data
   y[r==0] <- NA
   u <- u0  # endpoint with missing data
   u[r==0] <- NA
   data.frame(y, u, y0, u0, a, w1, w2)
}
