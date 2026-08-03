## Cross-implementation checks for cate() with missing outcomes.
##
## Three independent routes to the same estimand are compared:
##   1. a hand-coded AIPW/IPMW reference
##   2. targeted::aipw(), which handles MAR through the treatment.model slot
##   3. cate() with `missing.model`
## see inst/slowtest for a comparison against the external AIPW package.

library("tinytest")

sim1 <- function(n = 1200, seed = 1) {
  set.seed(seed)
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a  <- rbinom(n, 1, plogis(-0.2 + 0.4 * w1))
  y_full <- 1 + a + w1 + 0.5 * w2 + rnorm(n)
  r <- rbinom(n, 1, plogis(0.6 + 0.5 * w1 + 0.4 * a))
  data.frame(y = ifelse(r == 1, y_full, NA_real_), a = a, w1 = w1, w2 = w2)
}
d <- sim1()

# AIPW reference implementation.
# For treatment level a the estimator is
#   P_n[ 1{A=a} R / (g_a(W) rho(W,a)) (Y - Q_a(W)) + Q_a(W) ]
# with second-order corrections for the estimated nuisance parameters.
aipw_reference <- function(d, level, second.order = TRUE) {
  r <- as.integer(!is.na(d$y))
  y0 <- ifelse(is.na(d$y), 0, d$y)             # zeroed; r removes the term
  ## treatment model g_level(W)
  gfit <- glm(I(a == level) ~ w1 + w2, data = d, family = binomial)
  g <- predict(gfit, type = "response")
  ## missingness model rho(W, A), fit on all rows, evaluated at A = level
  dr <- transform(d, R_ = r)
  rfit <- glm(R_ ~ a * (w1 + w2), data = dr, family = binomial)
  rho <- predict(rfit, newdata = transform(dr, a = level), type = "response")
  ## outcome model, fit on observed rows only, evaluated at A = level
  qfit <- lm(y ~ a * (w1 + w2), data = d[r == 1, ])
  q <- predict(qfit, newdata = transform(d, a = level))

  ind <- as.integer(d$a == level)
  k <- (ind * r) / (g * rho) * (y0 - q)
  score <- k + q
  est <- mean(score)
  ic <- score - est
  if (second.order) {
    adj_g <- -k / g * gfit$family$mu.eta(gfit$family$linkfun(g))
    ic <- ic + IC(gfit) %*% colMeans(model.matrix(gfit) * adj_g)
    adj_r <- -k / rho * rfit$family$mu.eta(rfit$family$linkfun(rho))
    ic <- ic + IC(rfit) %*% colMeans(model.matrix(rfit) * adj_r)
  }
  list(est = est, ic = as.vector(ic))
}

test_cate_missing_vs_reference <- function() {
  for (so in c(TRUE, FALSE)) {
    fit <- cate(cate.model = ~1,
                response.model = y ~ a * (w1 + w2),
                treatment.model = a ~ w1 + w2,
                missing.model  = ~ a * (w1 + w2),
                nfolds = 1, second.order = so,
                data = d)
    ref1 <- aipw_reference(d, level = 1, second.order = so)
    ref0 <- aipw_reference(d, level = 0, second.order = so)
    ## potential outcome means
    expect_equal(unname(coef(fit)["E[y(1)]"]), ref1$est, tolerance = 1e-10)
    expect_equal(unname(coef(fit)["E[y(0)]"]), ref0$est, tolerance = 1e-10)
    ## and the full influence functions
    expect_true(max(abs(IC(fit)[, 1] - ref1$ic)) < 1e-10)
    expect_true(max(abs(IC(fit)[, 2] - ref0$ic)) < 1e-10)
    ## ATE contrast and its influence function
    expect_equal(unname(coef(fit)["(Intercept)"]), ref1$est - ref0$est,
                 tolerance = 1e-10)
    expect_true(max(abs(IC(fit)[, 3] - (ref1$ic - ref0$ic))) < 1e-10)
    ## standard errors follow from the influence functions
    n <- nrow(d)
    se_ref <- c(sqrt(sum(ref1$ic^2)), sqrt(sum(ref0$ic^2)),
                sqrt(sum((ref1$ic - ref0$ic)^2))) / n
    expect_equal(unname(sqrt(diag(vcov(fit$estimate)))), se_ref,
                 tolerance = 1e-10)
  }
}
test_cate_missing_vs_reference()

test_cate_missing_reference_stratified <- function() {
  ## Same check with stratify = TRUE: the outcome model and the missingness
  ## model are both fit per arm. The second-order term for the arm-specific
  ## missingness fit needs the n/m rescaling described below.
  n <- nrow(d)
  ref <- function(level) {
    r <- as.integer(!is.na(d$y))
    y0 <- ifelse(is.na(d$y), 0, d$y)
    gfit <- glm(I(a == level) ~ w1 + w2, data = d, family = binomial)
    g <- predict(gfit, type = "response")
    dr <- transform(d, R_ = r)
    midx <- which(d$a == level)
    rfit <- glm(R_ ~ w1 + w2, data = dr[midx, ], family = binomial)
    rho <- predict(rfit, newdata = dr, type = "response")
    qfit <- lm(y ~ w1 + w2, data = d[r == 1 & d$a == level, ])
    q <- predict(qfit, newdata = d)
    ind <- as.integer(d$a == level)
    k <- (ind * r) / (g * rho) * (y0 - q)
    est <- mean(k + q)
    ic <- (k + q) - est
    ## treatment model (fit on the full sample)
    adj_g <- -k / g * gfit$family$mu.eta(gfit$family$linkfun(g))
    ic <- ic + IC(gfit) %*% colMeans(model.matrix(gfit) * adj_g)
    ## missingness model (fit on arm `level` only). IC() of that fit is
    ## normalised by the arm size m, so rescale by n/m and pad with zeros
    ## to get the corresponding full-sample influence function.
    adj_r <- -k / rho * rfit$family$mu.eta(rfit$family$linkfun(rho))
    xr <- model.matrix(~ w1 + w2, data = dr) * adj_r
    ic_r <- matrix(0, n, ncol(xr))
    ic_r[midx, ] <- IC(rfit) * (n / length(midx))
    ic <- ic + ic_r %*% colMeans(xr)
    list(est = est, ic = as.vector(ic))
  }
  fit <- cate(cate.model = ~1,
              response.model = y ~ w1 + w2,
              treatment.model = a ~ w1 + w2,
              missing.model  = ~ w1 + w2,
              stratify = TRUE, nfolds = 1,
              data = d)
  r1 <- ref(1); r0 <- ref(0)
  expect_equal(unname(coef(fit)["E[y(1)]"]), r1$est, tolerance = 1e-10)
  expect_equal(unname(coef(fit)["E[y(0)]"]), r0$est, tolerance = 1e-10)
  expect_true(max(abs(IC(fit)[, 1] - r1$ic)) < 1e-10)
  expect_true(max(abs(IC(fit)[, 2] - r0$ic)) < 1e-10)
}
test_cate_missing_reference_stratified()

test_cate_missing_stratify_matches_pooled <- function() {
  ## A pooled fit that is saturated in the treatment is algebraically the
  ## same estimator as the corresponding stratified fit. This pins down the
  ## second-order correction for the arm-specific missingness model: the
  ## two routes must agree exactly, including the influence functions.
  set.seed(4)
  n <- 2000
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a <- rbinom(n, 1, plogis(-0.2 + 0.8 * w1))
  ## deliberately misspecified outcome model below, which is when the
  ## second-order term is non-negligible
  y_full <- 1 + a + 2 * sin(3 * w1) + w1^2 + 0.5 * w2 + rnorm(n)
  r <- rbinom(n, 1, plogis(0.3 + 0.9 * w1 + 0.4 * a))
  d <- data.frame(y = ifelse(r == 1, y_full, NA_real_), a = a,
                  w1 = w1, w2 = w2)

  pooled <- cate(cate.model = ~1,
                 response.model = y ~ a * (w1 + w2),
                 treatment.model = a ~ w1,
                 missing.model = ~ a * w1,
                 stratify = FALSE, data = d)
  strat <- cate(cate.model = ~1,
                response.model = y ~ w1 + w2,
                treatment.model = a ~ w1,
                missing.model = ~ w1,
                stratify = TRUE, data = d)
  expect_equal(unname(coef(pooled)), unname(coef(strat)), tolerance = 1e-10)
  expect_true(max(abs(pooled$data$pr[[1]] - strat$data$pr[[1]])) < 1e-10)
  expect_true(max(abs(IC(pooled) - IC(strat))) < 1e-9)
  expect_equal(unname(sqrt(diag(vcov(pooled$estimate)))),
               unname(sqrt(diag(vcov(strat$estimate)))), tolerance = 1e-10)
}
test_cate_missing_stratify_matches_pooled()

## cate() vs aipw()
## Estimating E[Y(1)] with a factorised weight g(W) * rho(W, 1) is the same
## as an AIPW mean using the combined indicator D = 1{A = 1, R = 1} with
## propensity P(D = 1 | W). With saturated (discrete) nuisance models the
## factorisation is exact, so the two routes must agree numerically.
sim_discrete <- function(n = 4000, seed = 3) {
  set.seed(seed)
  w <- factor(sample(1:4, n, replace = TRUE))
  a <- rbinom(n, 1, c(0.3, 0.5, 0.6, 0.7)[as.integer(w)])
  y_full <- c(0, 1, 2, 3)[as.integer(w)] + 2 * a + rnorm(n)
  r <- rbinom(n, 1, c(0.5, 0.6, 0.7, 0.8)[as.integer(w)] - 0.1 * a)
  data.frame(
    y  = ifelse(r == 1, y_full, NA_real_),
    yd = ifelse(a == 1 & r == 1, y_full, NA_real_), # observed iff D = 1
    a = a, w = w
  )
}
d2 <- sim_discrete()

test_cate_missing_equals_aipw <- function() {
  for (so in c(TRUE, FALSE)) {
    ce <- cate(cate.model = ~1, response.model = y ~ w,
               treatment.model = a ~ w, missing.model = ~ w,
               stratify = TRUE, second.order = so, data = d2)
    ai <- aipw(response.model = yd ~ w, propensity.model = R_ ~ w,
               data = d2, second.order = so)
    ## point estimate
    expect_equal(unname(coef(ce)["E[y(1)]"]),
                 unname(coef(ai$estimate)["(Intercept)"]),
                 tolerance = 1e-8)
    ## influence function
    expect_true(max(abs(IC(ce)[, 1] - IC(ai$estimate)[, 1])) < 1e-6)
    ## and hence the standard error
    se_ce <- sqrt(diag(vcov(estimate(ce, keep = 1))))
    se_ai <- sqrt(diag(vcov(ai$estimate)))[1]
    expect_equal(unname(se_ce), unname(se_ai), tolerance = 1e-6)
  }
}
test_cate_missing_equals_aipw()

test_cate_missing_mar_mean_equals_aipw <- function() {
  ## With a single (degenerate) treatment arm the target reduces to the
  ## plain MAR mean E[Y], which is exactly what aipw() estimates.
  set.seed(11)
  n <- 2000
  w1 <- rnorm(n); w2 <- rnorm(n)
  r <- rbinom(n, 1, plogis(0.4 + 0.7 * w1 - 0.3 * w2))
  y_full <- 1 + w1 + 0.5 * w2 + rnorm(n)
  d <- data.frame(y = ifelse(r == 1, y_full, NA_real_),
                  w1 = w1, w2 = w2, trt = 1L)
  ta <- aipw(y ~ w1 + w2, propensity.model = R_ ~ w1 + w2, data = d)
  ## degenerate propensity (all units in one arm) => second.order = FALSE
  tc <- suppressWarnings(
    cate(cate.model = ~1, response.model = y ~ w1 + w2,
         treatment.model = trt ~ 1, missing.model = ~ w1 + w2,
         contrast = 1L, stratify = TRUE, second.order = FALSE, data = d)
  )
  expect_equal(unname(coef(tc)["E[y(1)]"]),
               unname(coef(ta$estimate)["(Intercept)"]),
               tolerance = 1e-8)
  ## both should sit near the truth E[Y] = 1
  expect_equal(unname(coef(ta$estimate)["(Intercept)"]), 1, tolerance = 0.1)
}
test_cate_missing_mar_mean_equals_aipw()

## Supplying the true missingness probabilities (through a saturated model
## on the generating linear predictor) should remove the complete-case bias.
test_cate_missing_beats_complete_case <- function() {
  ipmw <- cate(cate.model = ~1, response.model = y ~ a * (w1 + w2),
               treatment.model = a ~ w1 + w2,
               missing.model  = ~ a * (w1 + w2), data = d)
  cc <- cate(cate.model = ~1, response.model = y ~ a * (w1 + w2),
             treatment.model = a ~ w1 + w2, data = d[!is.na(d$y), ])
  ## truth: E[Y(1)] = 2, E[Y(0)] = 1
  expect_true(abs(coef(ipmw)["E[y(1)]"] - 2) < abs(coef(cc)["E[y(1)]"] - 2))
  expect_true(abs(coef(ipmw)["E[y(0)]"] - 1) < abs(coef(cc)["E[y(0)]"] - 1))
  expect_equal(unname(coef(ipmw)["E[y(1)]"]), 2, tolerance = 0.1)
  expect_equal(unname(coef(ipmw)["E[y(0)]"]), 1, tolerance = 0.1)
}
test_cate_missing_beats_complete_case()
