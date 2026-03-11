library("tinytest")
library("carts")
library("lava")
library("future.apply")

test_moi <- function() {

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
  par0 <- c(-0.8, 0.1, 0.3, -0.4, 0.4, 0.1, 0.03, -0.4, -0.2)
  outcome <- setargs(
    outcome_binary,
    mean = ~ 1 + w1 + w2 + w3 + a + a:w1 + a:w3 +a:w2 + a:w2:w3,
    par = par0
  )

  ## true outcome model
  q0 <- function(a, w1, w2, w3) {
    apply(cbind(1, w1, w2, w3, a, a*w1, a*w3, a*w2, a*w2*w3), MARGIN = 1, function(x) expit(sum(x * par0)))
  }

  ## tmp_d <- covariate(1e6)
  ## tmp_d <- cbind(tmp_d, outcome(tmp_d))
  ## tmp <- glm(y ~ w1 + w2 + w3 + a + a:w1 + a:w3 +a:w2 + a:w2:w3, family = binomial(), data = tmp_d)
  ## predict(tmp, newdata = data.frame(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3)), type = "response")
  ## q0(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3))
  ## rm(tmp_d, tmp)

  ## coarsening distribution
  coarsening_par0 <- c(1.2,-0.2, 0.7, 0.2, 0.3, 0.3, 0.4, 0.2)
  coarsening = setargs(
    outcome_binary,
    mean = ~ 1 + w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3,
    par = coarsening_par0 # coef order as defined by the above formula
  )

  ## true coarsening model
  s0 <- function(a, w1, w2, w3) {
    apply(cbind(1, w1, w2, a, a*w1, a*w2, a*w3, a*w2*w3), MARGIN = 1, function(x) lava::expit(sum(x * coarsening_par0)))
  }

  ## tmp_d <- covariate(1e6)
  ## tmp_d <- cbind(tmp_d, delta = coarsening(tmp_d)$y)
  ## tmp <- glm(delta ~ w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3, family = binomial(), data = tmp_d)
  ## predict(tmp, newdata = data.frame(a = c(0,1), w1 = c(1,2), w2 = c(0,1), w3 = c(0,1)), type = "response")
  ## s0(a = c(0,1), w1 = c(1,2), w2 = c(0,1), w3 = c(0,1))

  ## approximating true target parameter under imputation
  approx_target <- function(n = 1e4) {
    covar <- covariate(n)
    covar$delta <- coarsening(covar)$y
    covar$y <- outcome(covar)$y

    ## approximating true value of P(\Delta = 1|A=a), a = 0,1
    PDeltaA1 <- mean(s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    PDeltaA0 <- mean(s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))


    ## approximating the true imputation model u:  ~ w1 + w2 in a = 0
    covar$q <- q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)
    imp0 <- learner_glm(q ~ w1 + w2, family = quasibinomial())
    imp0$estimate(data = covar[covar$delta == 1, ])
    covar$u <- imp0$predict(covar, type = "response")
    ## glm(y ~ w1 + w2, data = covar[covar$delta == 1 & covar$a == 0,], family = binomial())

    ## approximating E[(1-\Delta) U(X)|A=a], a = 0,1
    E1DUA1 <- mean((1 - s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * covar$u)
    E1DUA0 <- mean((1 - s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * covar$u)

    ## calculating E[U|A=a, \Delta = 0]
    EUD0A1 <- E1DUA1 / (1-PDeltaA1)
    EUD0A0 <- E1DUA0 / (1-PDeltaA0)

    c(EUD0A1, EUD0A0)
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 4)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)

  ## setup trial
  trial <- Trial$new(covariates = covariate,
                     outcome = \(data) outcome(data) *
                                       ifelse(coarsening(data) == 1, 1, NA))

  ## run and report the simulation study
  onerun <- function(n) {
    data <- trial$simulate(n)
    id <- 1:n
    delta <- !is.na(data$y)

    model <- moi(data = data,
                 id = id,
                 delta = delta,
                 treatment.model = learner_glm(a ~ 1, family = binomial()),
                 imputation.model = learner_glm(y ~ w1 + w2, family = binomial()),
                 imputation.subset = "!is.na(y) & a == 0")
    est <- model$estimate

    model_aug <- moi(
      data = data,
      id = id,
      delta = delta,
      treatment.model = learner_glm(a ~ 1, family = binomial()),
      imputation.model = learner_glm(y ~ w1 + w2, family = binomial()),
      imputation.subset = "!is.na(y) & a == 0",
      imputation.augmentation = TRUE,
      missing.model =  learner_glm(
        ~ 1 + w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3,
        family = binomial()
      )
    )
    est_aug <- model_aug$estimate

    merge(est, est_aug)
  }

  plan(tweak("multicore", workers = 7))
  res <- sim(onerun, R = 1e4, seed = 1, args = list(n = 1e3))
  sumres <- summary(res,
                    estimate = 1:4,
                    se = 5:8,
                    true = targets0)

  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance=0.0012))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.025))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.01))

}

test_moi()

test_moi_aug <- function() {

  ## covariate distribution
  covariate <- function(n) {
    data.frame(
      a = rbinom(n, 1, 0.5),
      w1 = rnorm(n = n, mean = 0, sd = 4),
      w2 = rbinom(n, 1, 0.8),
      w3 = rbinom(n, 1, 0.5)
    )
  }

  ## outcome distribution
  par0 <- c(-0.8, 0.1, 0.3, -0.4, 0.4, 0.1, 0.03, -0.4, -0.2)
  outcome <- setargs(
    outcome_binary,
    mean = ~ 1 + w1 + w2 + w3 + a + a:w1 + a:w3 +a:w2 + a:w2:w3,
    par = par0
  )

  ## true outcome model
  q0 <- function(a, w1, w2, w3) {
    apply(cbind(1, w1, w2, w3, a, a*w1, a*w3, a*w2, a*w2*w3), MARGIN = 1, function(x) expit(sum(x * par0)))
  }

  ## tmp_d <- covariate(1e6)
  ## tmp_d <- cbind(tmp_d, outcome(tmp_d))
  ## tmp <- glm(y ~ w1 + w2 + w3 + a + a:w1 + a:w3 +a:w2 + a:w2:w3, family = binomial(), data = tmp_d)
  ## predict(tmp, newdata = data.frame(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3)), type = "response")
  ## q0(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3))
  ## rm(tmp_d, tmp)

  ## coarsening distribution
  coarsening_par0 <- c(1.2,-0.2, 0.7, 0.2, 0.3, 0.3, 0.4, 0.2)
  coarsening = setargs(
    outcome_binary,
    mean = ~ 1 + w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3,
    par = coarsening_par0 # coef order as defined by the above formula
  )

  ## true coarsening model
  s0 <- function(a, w1, w2, w3) {
    apply(cbind(1, w1, w2, a, a*w1, a*w2, a*w3, a*w2*w3), MARGIN = 1, function(x) lava::expit(sum(x * coarsening_par0)))
  }

  ## tmp_d <- covariate(1e6)
  ## tmp_d <- cbind(tmp_d, delta = coarsening(tmp_d)$y)
  ## tmp <- glm(delta ~ w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3, family = binomial(), data = tmp_d)
  ## predict(tmp, newdata = data.frame(a = c(0,1), w1 = c(1,2), w2 = c(0,1), w3 = c(0,1)), type = "response")
  ## s0(a = c(0,1), w1 = c(1,2), w2 = c(0,1), w3 = c(0,1))

  ## approximating true target parameter under imputation
  approx_target <- function(n = 1e4) {
    covar <- covariate(n)
    covar$delta <- coarsening(covar)$y
    covar$y <- outcome(covar)$y

    ## approximating true value of P(\Delta = 1|A=a), a = 0,1
    PDeltaA1 <- mean(s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    PDeltaA0 <- mean(s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))


    ## approximating the true imputation model u:  ~ a * (w1 + w2)
    covar$q <- q0(a = covar$a, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)
    imp0 <- learner_glm(q ~ a * (w1 + w2), family = quasibinomial())
    imp0$estimate(data = covar[covar$delta == 1, ])

    covar$a <- 1
    covar$u1 <- imp0$predict(covar, type = "response")
    covar$a <- 0
    covar$u0 <- imp0$predict(covar, type = "response")
    ## glm(y ~ a * (w1 + w2), data = covar[covar$delta == 1,], family = binomial())

    ## approximating E[(1-\Delta) U(X)|A=a], a = 0,1
    E1DUA1 <- mean((1 - s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * covar$u1)
    E1DUA0 <- mean((1 - s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * covar$u0)

    ## calculating E[U|A=a, \Delta = 0]
    EUD0A1 <- E1DUA1 / (1-PDeltaA1)
    EUD0A0 <- E1DUA0 / (1-PDeltaA0)

    c(EUD0A1, EUD0A0)
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 4)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)

  ## setup trial
  trial <- Trial$new(covariates = covariate,
                     outcome = \(data) outcome(data) *
                                       ifelse(coarsening(data) == 1, 1, NA))

  ## run and report the simulation study
  onerun <- function(n) {
    data <- trial$simulate(n)
    id <- 1:n
    delta <- !is.na(data$y)

    model <- moi(data = data,
                 id = id,
                 delta = delta,
                 treatment.model = learner_glm(a ~ 1, family = binomial()),
                 imputation.model = learner_glm(y ~ a * (w1 + w2), family = binomial()),
                 imputation.subset = "!is.na(y)")
    est <- model$estimate

    model_aug <- moi(
      data = data,
      id = id,
      delta = delta,
      treatment.model = learner_glm(a ~ 1, family = binomial()),
      imputation.model = learner_glm(y ~ a * (w1 + w2), family = binomial()),
      imputation.subset = "!is.na(y)",
      imputation.augmentation = TRUE,
      missing.model =  learner_glm(
        ~ 1 + w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3,
        family = binomial()
      )
    )
    est_aug <- model_aug$estimate

    merge(est, est_aug)
  }

  plan(tweak("multicore", workers = 7))
  res <- sim(onerun, R = 2e4, seed = 1, args = list(n = 2e3))
  sumres <- summary(res,
                    estimate = 1:4,
                    se = 5:8,
                    true = targets0)

  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance = 0.00025))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.0075))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.01))

}

test_moiate_continuous <- function() {

  ## covariate distribution
  covariate = function(n) {
    data.frame(
      a = rbinom(n, 1, 0.5),
      w1 = rnorm(n),
      w2 = rbinom(n, 1, 0.8),
      w3 = rbinom(n, 1, 0.5)
    )
  }

  ##  outcome distribution
  par0 <- c(10, -3, 0.1, 0.03, -0.04, 0.02)
  outcome = setargs(
    outcome_continuous,
    mean = ~ 1 + a + w1 + w2 + w3 + w2:w3,
    par = par0,
    # coef order as defined by the above formula
    sd = 1
  )

  ## tmp_d <- covariate(10000) %>% cbind(., outcome(.))
  ## tmp <- lm(y ~ a + w1 + w2 + w3 + w2:w3, data = tmp_d)

  ## true outcome model
  q0 <- function(a, w1, w2, w3) {
    apply(cbind(1, a, w1, w2, w3, w2*w3), MARGIN = 1, function(x) sum(x * par0))
  }

  ## predict(tmp, newdata = data.frame(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3)))
  ## q0(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3))

  ## coarsening distribution
  coarsening_par0 <- c(1.5, 0.2, 0.3, 0.3, 0.4, 0.2)
  coarsening = setargs(
    outcome_binary,
    mean = ~ 1 + a + w1 + w2 + w3 + w2:w3,
    par = coarsening_par0 # coef order as defined by the above formula
  )

  ## true coarsening model
  s0 <- function(a, w1, w2, w3) {
    apply(cbind(1, a, w1, w2, w3, w2*w3), MARGIN = 1, function(x) lava::expit(sum(x * coarsening_par0)))
  }

  ## approximate true target parameters under imputation
  approx_target <- function(n = 1e4) {
    covar <- covariate(n)
    ED1Y1 <- mean(q0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3) * s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    ED0Y0 <- mean(q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3) * s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))

    PDeltaA1 <- mean(s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    PDeltaA0 <- mean(s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))

    E1DUA1 <- mean((1 - s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    E1DUA0 <- mean((1 - s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))

    EUD0A1 <- E1DUA1 / (1-PDeltaA1)
    EUD0A0 <- E1DUA0 / (1-PDeltaA0)

    targets <- c(
      ED1Y1,
      ED0Y0,
      1 - PDeltaA1,
      1 - PDeltaA0,
      EUD0A1,
      EUD0A0,
      (ED1Y1 + E1DUA1) - (ED0Y0 + E1DUA0)
    )

    return(targets)
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 4)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)

  ## setup trial
  trial <- Trial$new(covariates = covariate,
                     outcome = \(data) outcome(data) *
                                       ifelse(coarsening(data) == 1, 1, NA))

  ## run and report the simulation study
  onerun <- function(n) {
    data <- trial$simulate(n)
    id <- 1:n
    delta <- !is.na(data$y)

    est <- moiate(
      data = data,
      treatment.model = a ~ 1,
      response.model = learner_glm(y ~ a),
      missing.model = learner_glm(delta ~ a, family = binomial()),
      imputation.model = learner_glm(y ~ w1 + w2 + w3 + w2:w3),
      imputation.subset = "!is.na(y) & a == 0",
      return.all = TRUE
    )

    est_aug <- moi(
      data = data,
      id = id,
      delta = delta,
      treatment.model = learner_glm(a ~ 1, family = binomial()),
      missing.model = learner_glm(delta ~ a, family = binomial()),
      imputation.model = learner_glm(y ~ w1 + w2 + w3 + w2:w3),
      imputation.subset = "!is.na(y) & a == 0",
      imputation.augmentation = TRUE
    )

    merge(est, est_aug$estimate)
  }

  plan(tweak("multicore", workers = 4))
  res <- sim(onerun, R = 1e4, seed = 1, args = list(n = 1e3))
  sumres <- summary(res,
                    estimate = 1:9,
                    se = 10:18,
                    true = c(targets0, targets0[5:6]))


  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance=0.0025))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.02))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.01))
}

test_moiate_continuous()

test_moiate_binary <- function() {

  ## covariates and treatment distribution
  covariate <- function(n) {
    data.frame(
      a = rbinom(n, 1, 2/3),
      w1 = rnorm(n),
      w2 = rbinom(n, 1, 0.8),
      w3 = rbinom(n, 1, 0.5)
    )
  }
  ## outcome distribution
  par0 <- c(-0.8, 0.1, 0.3, -0.4, 0.6, 0.1, 0.03, -0.4, -0.2)
  mean0 <-  ~ 1 + w1 + w2 + w3 + a + a:w1 + a:w3 +a:w2 + a:w2:w3
  outcome <- setargs(
    outcome_binary,
    mean = mean0,
    par = par0
  )
  ## true outcome model
  q0 <- function(a, w1, w2, w3) {
    apply(cbind(1, w1, w2, w3, a, a*w1, a*w3, a*w2, a*w2*w3), MARGIN = 1, function(x) expit(sum(x * par0)))
  }

  ## tmp_d <- covariate(1e3)
  ## tmp_d <- cbind(tmp_d, outcome(tmp_d))
  ## tmp <- glm(y ~ w1 + w2 + w3 + a + a:w1 + a:w3 +a:w2 + a:w2:w3, family = binomial(), data = tmp_d)
  ## predict(tmp, newdata = data.frame(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3)), type = "response")
  ## q0(a = c(0,1), w1 = c(1,2), w2 = c(1,2), w3 = c(1,3))
  ## rm(tmp_d, tmp)

  ## coarsening distribution
  coarsening_par0 <- c(1.2,-0.2, 0.7, 0.2, 0.3, 0.3, 0.4, 0.2)
  coarsening_mean0 <-  ~ 1 + w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3
  coarsening = setargs(
    outcome_binary,
    mean = coarsening_mean0,
    par = coarsening_par0 # coef order as defined by the above formula
  )

  # true coarsening model
  s0 <- function(a, w1, w2, w3) {
    apply(cbind(1, w1, w2, a, a*w1, a*w2, a*w3, a*w2*w3), MARGIN = 1, function(x) lava::expit(sum(x * coarsening_par0)))
  }

  ## tmp_d <- covariate(1e3)
  ## tmp_d <- cbind(tmp_d, delta = coarsening(tmp_d)$y)
  ## tmp <- glm(delta ~ w1 + w2 + a + a:w1 + a:w2 + a:w3 + a:w2:w3, family = binomial(), data = tmp_d)
  ## predict(tmp, newdata = data.frame(a = c(0,1), w1 = c(1,2), w2 = c(0,1), w3 = c(0,1)), type = "response")
  ## s0(a = c(0,1), w1 = c(1,2), w2 = c(0,1), w3 = c(0,1))

  ## approximating the true imputation model u:  ~ w1 + w2 in a = 0
  imputation_form0 <- y ~ w1 + w2

  ## approximating true target parameters under imputation
  approx_target <- function(n = 1e4) {
    covar <- covariate(n)
    covar$delta <- coarsening(covar)$y
    covar$y <- outcome(covar)$y

    ## approximating E[\Delta Y|A=a], a = 0,1
    ED1Y1 <- mean(q0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3) * s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    ED0Y0 <- mean(q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3) * s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))

    ## approximating true value of P(\Delta = 1|A=a), a = 0,1
    PDeltaA1 <- mean(s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))
    PDeltaA0 <- mean(s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3))


    covar$q <- q0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)
    imp0 <- learner_glm(
      formula = reformulate(attr(terms(imputation_form0), "term.labels"), response = "q"),
      family = quasibinomial())
    imp0$estimate(data = covar[covar$delta == 1, ])
    covar$u <- imp0$predict(covar, type = "response")
    ## glm(y ~ w1 + w2, data = covar[covar$delta == 1 & covar$a == 0,], family = binomial())

    ## approximating E[(1-\Delta) U(X)|A=a], a = 0,1
    E1DUA1 <- mean((1 - s0(a = 1, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * covar$u)
    E1DUA0 <- mean((1 - s0(a = 0, w1 = covar$w1, w2 = covar$w2, w3 = covar$w3)) * covar$u)

    ## calculating E[U|A=a, \Delta = 0] for convenience
    EUD0A1 <- E1DUA1 / (1-PDeltaA1)
    EUD0A0 <- E1DUA0 / (1-PDeltaA0)

    ## mean(covar[covar$delta == 0 & covar$a == 1,]$u)
    ## mean(covar[covar$delta == 0 & covar$a == 0,]$u)

    targets <- c(
      ED1Y1,
      ED0Y0,
      1 - PDeltaA1,
      1 - PDeltaA0,
      EUD0A1,
      EUD0A0,
      (ED1Y1 + E1DUA1) - (ED0Y0 + E1DUA0)
    )

    return(targets)
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 4)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)

  ## setup trial
  trial <- Trial$new(covariates = covariate,
                     outcome = \(data) outcome(data) *
                                       ifelse(coarsening(data) == 1, 1, NA),
                     estimators = list(
                       `onestep` = setargs(
                         moiate,
                         treatment.model = a ~ 1,
                         response.model = learner_glm(
                           formula = reformulate(attr(terms(mean0), "term.labels"), response = "y"),
                           family = binomial()
                         ),
                         missing.model = reformulate(attr(terms(coarsening_mean0), "term.labels"), response = "delta"),
                         imputation.model = learner_glm(imputation_form0,
                                                        family = binomial()),
                         imputation.subset = "!is.na(y) & a == 0",
                         return.all = TRUE
                       )
                     ))

  ## test
  ## d <- trial$simulate(1e3)
  ## moiate(data = d,
  ##        treatment.model = a ~ 1,
  ##        response.model = learner_glm(
  ##          formula = reformulate(attr(terms(mean0), "term.labels"), response = "y"),
  ##          family = binomial()
  ##        ),
  ##        missing.model = reformulate(attr(terms(coarsening_mean0), "term.labels"), response = "delta"),
  ##        imputation.model = learner_glm(imputation_form0,
  ##                                       family = binomial()),
  ##        imputation.subset = "!is.na(y) & a == 0",
  ##        return.all = TRUE)
  ## tmp <- trial$estimators("onestep")
  ## tmp(data = d)

  ## run and report the simulation study
  onerun <- function(n) {
    data <- trial$simulate(n)
    est <- trial$estimators("onestep")(data = data)

    out <- c(
      est = coef(est),
      se = sqrt(diag(vcov(est)))
    )
    return(out)
  }

  plan(tweak("multicore", workers = 7))
  res <- sim(onerun, R = 1e4, seed = 1, args = list(n = 1e3))
  sumres <- summary(res,
                    estimate = 1:7,
                    se = 8:14,
                    true = targets0)


  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance=0.0012))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.025))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.01))
}

test_moiate_binary()

test_moi_postrand <- function() {

  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    y <- 1 + a + x - a * x + w + a * w + z + rnorm(n)           # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }

  ## true target parameters,
  ##
  ## E[U(X,A,Z)|A = a, \Delta = 0],
  ## U(X,A,Z): linear model of x, z among the non-missing in
  ## the reference treatment arm, a = 0

  approx_target <- function(n = 1e5) {
    data <- simdata(n = n, full = TRUE)
    ## approximating the true imputation model u: y ~ x + z in a = 0
    data$q <- with(data = data, 1 + a + x - a * x + w + a * w + z)
    imp <- learner_glm(q ~ x + z, family = gaussian())
    imp$estimate(data = data[!is.na(data$y) & data$a == 0,])
    data$u <- imp$predict(data, type = "response")

    delta <- !is.na(data$y)
    delta_y <- ifelse(delta, data$y, 0)


    tt1 <- sapply(
      X = c(1,0),
      FUN = function(a) {
        mean(delta_y[data$a == a])
      }
    )

    tt2 <- sapply(
      X = c(1,0),
      FUN = function(a) {
        mean(1 - delta[data$a == a])
      }
    )

    tt3 <- sapply(
      X = c(1,0),
      FUN = function(a) {
        mean(data[is.na(data$y) & data$a == a, ]$u)
      }
    )

    ate <- tt1 + tt2 * tt3
    ate <- ate[1] - ate[2]

    c(tt1,
      tt2,
      tt3,
      ate)
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 4)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)

  ## target0 <- c(2.768475, 1.449266)
  ## target0[1] - target0[2]

  onerun <- function(n) {
    data <- simdata(n = n, full = FALSE)
    id <- 1:n
    delta <- !is.na(data$y)

    model <- moiate(
      data = data,
      response.model = learner_glm(y ~ a * x),
      missing.model = learner_glm(~ a * x, family = binomial()),
      imputation.model = learner_glm(formula = y  ~ x + z),
      imputation.subset = "!is.na(y) & a == 0",
      treatment.model = learner_glm(formula = a ~ 1, family = binomial()),
      imputation.augmentation = FALSE,
      return.all = TRUE
    )

    model_aug <- moi(
      data = data,
      id = id,
      delta = delta,
      imputation.model = learner_glm(formula = y  ~ x + z),
      imputation.subset = "!is.na(y) & a == 0",
      treatment.model = learner_glm(formula = a ~ 1, family = binomial()),
      imputation.augmentation = TRUE,
      imputation.augmentation.model = learner_glm(y ~ x),
      missing.model = learner_glm(~ x, family = binomial())
    )

    merge(model, model_aug$estimate)
  }

  plan(tweak("multicore"), workers = 7)
  res <- sim(onerun, R = 2e4, seed = 1, args = list(n = 2e3))
  sumres <- summary(res, estimate = 1:9, se = 10:18, true = c(targets0, targets0[5:6]))

  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance=0.0015))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.01))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.0025))

}

test_moi_postrand()

test_rubins_rule <- function() {

  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    y <- 1 + a + x - a * x + w + a * w + z + rnorm(n)           # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }

  ## target: E[\tilde Y | A = a]
  approx_target <- function(n = 1e5) {
    data <- simdata(n = n, full = TRUE)
    ## approximating the true imputation model u: y ~ a + x
    data$q <- with(data = data, 1 + a + x - a * x + w + a * w + z)
    imp <- learner_glm(q ~ a + x, family = gaussian())
    imp$estimate(data = data[!is.na(data$y),])
    data$u <- imp$predict(data, type = "response")
    data$tilde_y <- ifelse(is.na(data$y), data$u, data$y)

    sapply(
      X = c(1,0),
      FUN = function(a) {
        mean(data[(data$a == a), ]$tilde_y)
      }
    )
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 4)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)

  multiple_imputation <- function(data, nrep = 500, increments = 100) {
    mi_sampling_fit = lm(formula = y ~ a + x,
                         data = data[!is.na(data$y), ]) # & data$a == 1
    delta <- is.na(data$y)
    imputed <- data
    nmis <- sum(delta)

    mi_res <- future_replicate(
      nrep,
      expr = {
        imputed[delta, "y"] <- rnorm(
          n = nmis,
          predict(mi_sampling_fit, newdata = data[delta, ]),
          summary(mi_sampling_fit)$sigma
        )
        model <- lm(formula = y ~ a + x, data = imputed)
        ate <- summary(pairs(emmeans::emmeans(model, ~ a), reverse = TRUE))
        c(estimate = ate$estimate, SE = ate$SE)
      },
      future.envir = environment()
    )

    indx <- split(1:nrep, ceiling(seq_along(1:nrep) / increments))

    cindx <- c()
    tmp <- c()
    for (j in seq_along(indx)) {
      tmp <- c(tmp, indx[[j]])
      cindx[[j]] <- tmp
    }

    res <- lapply(cindx,
                  function(i) {

                    tmp <- mice::pool.scalar(Q = mi_res["estimate", i],
                                             U = (mi_res["SE", i])^2,
                                             rule = 'rubin1987')
                    lava::estimate(coef = tmp$qbar, vcov = tmp$t)
                  })

    out <- res[[1]]
    if (length(res) > 1){
      for (i in 2:length(res)) {
        out <- merge(out, res[[i]])
      }
    }
    out
  }


  onerun <- function() {
    data <- simdata(1e3)
    mi_est <- multiple_imputation(data, nrep = 100, increments = 100)

    onestep_est <- targeted:::moiate(data = data,
                                     response.model = learner_glm(y ~ a + x),
                                     treatment.model = a ~ 1,
                                     missing.model = learner_glm(~ a + x, family = binomial()),
                                     imputation.model = learner_glm(y ~ a + x),
                                     imputation.subset = "!is.na(y)",
                                     return.all = FALSE)

    merge(mi_est, onestep_est)
  }
  plan("multicore")
  simres <- sim(onerun, R = 1e4, seed = 1)
  sumres <- summary(simres,
                    estimate = 1:2 ,
                    se = 3:4,
                    true = rep(targets0[1] - targets0[2]),2)

  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance=0.0025))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.01))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.0025))
}

test_rubins_rule()

test_moi_2 <- function() {

  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    y <- 1 + a + x - a * x + w + a * w + z + rnorm(n)           # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }

  ## target: E[U(X,A,Z;\theta) | \Delta = 0,  A = a]
  ## U: y ~ a + x
  approx_target <- function(n = 1e5) {
    data <- simdata(n = n, full = TRUE)
    ## approximating the true imputation model u: y ~ a + x
    data$q <- with(data = data, 1 + a + x - a * x + w + a * w + z)
    imp <- learner_glm(q ~ a + x, family = gaussian())
    imp$estimate(data = data[!is.na(data$y),])
    data$u <- imp$predict(data, type = "response")
    sapply(
      X = c(1,0),
      FUN = function(a) {
        mean(data[is.na(data$y) & data$a == a, ]$u)
      }
    )
  }

  set.seed(1)
  plan(tweak("multicore"), workers = 4)
  targets0 <- future_replicate(1e3, approx_target())
  targets0 <- rowMeans(targets0)

  onerun <- function() {
    data <- simdata(1e3)
    id <- 1:1e3
    delta <- !is.na(data$y)

    out <- targeted:::moi(data = data,
                          id = id,
                          delta = delta,
                          treatment.model = learner_glm(a ~ 1, family = binomial()),
                          imputation.model = learner_glm(y ~ a + x),
                          imputation.subset = "!is.na(y)")
    out$estimate

  }
  plan("multicore")
  simres <- sim(onerun, R = 1e4, seed = 1)
  sumres <- summary(simres, estimate = 1:2 , se = 3:4, true = targets0)

  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance=0.0025))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.01))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.0025))
}

test_moi_2()
