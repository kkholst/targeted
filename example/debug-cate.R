library(targeted)
future::plan("multicore")
progressr::handlers(global = FALSE)

nsim <- 1000
n <- 150

onerun <- function(n, true_ate, ...) {
  x <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  y <- 0.5 + true_ate * a + 1 * x + rnorm(n, sd = 1)
  d <- data.frame(y = y, a = a, x = x)

  fit1 <- cate(
    response.model = y ~ a,
    treatment.model = learner_glm(a ~ 1, family = binomial),
    nfolds = 1,
    rep = 1,
    data = d
  )

  outcome_model <- learner_sl(
    # learner_glm(y ~ a + x),
    # learner_glm(y ~ a * x),
    learner_glm(y ~ factor(a))
    # learner_gam(y ~ a + s(x))
  )

  fit2 <- cate(
    response.model = outcome_model,
    # treatment.model = ~ a,
    treatment.model = learner_glm(a ~ 1, family = binomial),
    calibration.model = ~1,
    nfolds = 40,
    rep = 1,
    data = d
    # var.type = "n",
    # second.order = FALSE
  )

  ci1 <- parameter(subset(fit1, 3))[,3:4]
  ci2 <- parameter(subset(fit2, 3))[,3:4]

  cover <- c(
    ci1[1] <= true_ate && true_ate <= ci1[2],
    subset(fit1, 3)$coefmat[[1]],
    subset(fit1, 3)$coefmat[[2]],
    ci2[1] <= true_ate && true_ate <= ci2[2],
    subset(fit2, 3)$coefmat[[1]],
    subset(fit2, 3)$coefmat[[2]]
  )
  return(cover)
}

res <- lava::sim(onerun, R = nsim, n = n, true_ate = 1)
print(res)
