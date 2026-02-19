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

    delta <- !is.na(data$y)
    A <- data$a

    est <- moi(data = data,
               delta = delta,
               A = A,
               levels = c(1,0),
               learner = learner_glm(y ~ w1 + w2, family = binomial()),
               subset = "!is.na(y) & a == 0")

    out <- c(
      est = coef(est),
      se = sqrt(diag(vcov(est)))
    )
    return(out)
  }

  plan(tweak("multicore", workers = 7))
  res <- sim(onerun, R = 1e4, seed = 1, args = list(n = 1e3))
  sumres <- summary(res,
                    estimate = 1:2,
                    se = 3:4,
                    true = targets0)

  ## test bias, SE/SD and coverage within a given tolerance
  lapply(sumres["Bias",], function(x) expect_equivalent(x, 0, tolerance=0.0012))
  lapply(sumres["SE/SD",], function(x) expect_equivalent(x, 1, tolerance = 0.025))
  lapply(sumres["Coverage",], function(x) expect_equivalent(x, 0.95, tolerance = 0.01))

}

test_moi()



test_moiate_binary <- function() {

  ## covariates and treatment distribution
  covariate <- function(n) {
    data.frame(
      a = rbinom(n, 1, 0.5),
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
                         propensity.model = a ~ 1,
                         response.model = learner_glm(
                           formula = reformulate(attr(terms(mean0), "term.labels"), response = "y"),
                           family = binomial()
                         ),
                         missing.model = reformulate(attr(terms(coarsening_mean0), "term.labels"), response = "delta"),
                         imputation.model = learner_glm(imputation_form0,
                                                        family = binomial()),
                         imputation.subset = "!is.na(y) & a == 0",
                         transform = NULL,
                         back.transform = NULL,
                         return.all = TRUE
                       )
                     ))

  ## test
  ## d <- trial$simulate(1e3)
  ## moiate(data = d,
  ##        propensity.model = a ~ 1,
  ##        response.model = learner_glm(
  ##          formula = reformulate(attr(terms(mean0), "term.labels"), response = "y"),
  ##          family = binomial()
  ##        ),
  ##        missing.model = reformulate(attr(terms(coarsening_mean0), "term.labels"), response = "delta"),
  ##        imputation.model = learner_glm(imputation_form0,
  ##                                       family = binomial()),
  ##        imputation.subset = "!is.na(y) & a == 0",
  ##        transform = NULL,
  ##        back.transform = NULL,
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
