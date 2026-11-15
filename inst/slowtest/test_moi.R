library("tinytest")
library("carts")
library("targeted")
library("lava")
library("future.apply")

## Simulation study for the direct one-step imputation estimator
## (supplementary material C of Nordland et al. 2026), exercised through
## moi(..., missing.model = NULL). Checks bias, SE/SD and coverage against
## approximate true target parameters, alongside the sub-target estimator.

test_moi_direct_continuous <- function() {

  ## covariate distribution
  covariate <- function(n) {
    data.frame(
      a = rbinom(n, 1, 0.5),
      w1 = rnorm(n),
      w2 = rbinom(n, 1, 0.8),
      w3 = rbinom(n, 1, 0.5)
    )
  }

  ## outcome distribution
  par0 <- c(10, -3, 0.1, 0.03, -0.04, 0.02)
  outcome <- setargs(
    outcome_continuous,
    mean = ~ 1 + a + w1 + w2 + w3 + w2:w3,
    par = par0, # coef order as defined by the above formula
    sd = 1
  )

  ## true outcome model
  q0 <- function(a, w1, w2, w3) {
    apply(cbind(1, a, w1, w2, w3, w2 * w3), MARGIN = 1,
          function(x) sum(x * par0))
  }

  ## coarsening distribution
  coarsening_par0 <- c(1.5, 0.2, 0.3, 0.3, 0.4, 0.2)
  coarsening <- setargs(
    outcome_binary,
    mean = ~ 1 + a + w1 + w2 + w3 + w2:w3,
    par = coarsening_par0 # coef order as defined by the above formula
  )

  ## true coarsening model
  s0 <- function(a, w1, w2, w3) {
    apply(cbind(1, a, w1, w2, w3, w2 * w3), MARGIN = 1,
          function(x) lava::expit(sum(x * coarsening_par0)))
  }

  ## approximate true target parameters under imputation
  approx_target <- function(n = 1e4) {
    covar <- covariate(n)
    ED1Y1 <- mean(q0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3) *
                  s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    ED0Y0 <- mean(q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3) *
                  s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))

    E1DUA1 <- mean((1 - s0(a = 1, w1 = covar$w1, w2 = covar$w2,
                           w3 = covar$w3)) *
                   q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    E1DUA0 <- mean((1 - s0(a = 0, w1 = covar$w1, w2 = covar$w2,
                           w3 = covar$w3)) *
                   q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))

    c(ED1Y1 + E1DUA1, ED0Y0 + E1DUA0)
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 6)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)
  plan("sequential")

  ## setup trial
  trial <- Trial$new(
    covariates = covariate,
    outcome = \(data) outcome(data) *
      ifelse(coarsening(data) == 1, 1, NA)
  )

  onerun <- function(n) {
    data <- trial$simulate(n)

    ## sub-target (indirect) estimator, with imputation augmentation
    est_indirect <- moi(
      response.model = learner_glm(y ~ a * (w1 + w2)),
      treatment.model = a ~ 1,
      imputation.model = learner_glm(y ~ w1 + w2 + w3 + w2:w3),
      imputation.augmentation.model = learner_glm(~ a * (w1 + w2)),
      imputation.subset = "!is.na(y) & a == 0",
      missing.model = learner_glm(delta ~ a + w1 + w2 + w3 + w2:w3,
                                  family = binomial()),
      data = data,
      type = "indirect"
    )
    est_indirect <- estimate(est_indirect$estimate)[c(1, 2)]

    ## direct one-step imputation estimator
    est_direct <- moi(
      response.model = learner_glm(y ~ a * (w1 + w2)),
      treatment.model = a ~ 1,
      imputation.model = learner_glm(y ~ w1 + w2 + w3 + w2:w3),
      imputation.subset = "!is.na(y) & a == 0",
      data = data,
      type = "direct"
    )
    est_direct <- estimate(est_direct$estimate)[c(1, 2)]

    c(est_indirect, est_direct)
  }

  library(progressr)
  handlers(global = TRUE)
  plan(tweak("multicore", workers = 6))
  res <- sim(onerun, R = 1e4, seed = 1, args = list(n = 1e3))
  plan("sequential")

  sumres <- summary(res,
                    estimate = 1:4,
                    se = 5:8,
                    true = c(targets0, targets0))

  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias", ],
         function(x) expect_equivalent(x, 0, tolerance = 0.0025))
  lapply(sumres["SE/SD", ],
         function(x) expect_equivalent(x, 1, tolerance = 0.02))
  lapply(sumres["Coverage", ],
         function(x) expect_equivalent(x, 0.95, tolerance = 0.01))
}

test_moi_direct_continuous()
