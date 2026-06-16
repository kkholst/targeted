
test_moi_missing <- function() {

  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    y <- 1 + a + x - a * x + w + a * w + z + rnorm(n)           # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(id = n:1, y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }

  data <- simdata(1e3)
  delta <- !is.na(data$y)

  ghat <- mean(data$a)
  pred_degen <- function(object = NULL, newdata, type = NULL) {
    a <- newdata[ , "a"]
    x <- newdata[ , "x"]
    z <- newdata[ , "z"]

    a * (x + z) + (1 - a) * (x^2 - z)
  }
  uhat <- pred_degen(newdata = data)


  learner_degen <- learner$new(estimate = function(y, x){"degenerate"},
                               formula = ~ .,
                               predict = pred_degen)

  tmp <- learner_degen
  class(tmp) <- c("learner_glm", class(tmp))

  out <- targeted:::moi_missing(data = data,
                                delta = delta,
                                treatment.model = learner_glm(a ~ 1, family = binomial()),
                                imputation.model = tmp,
                                imputation.subset = "!is.na(y)")

}

test_moi_nfolds <- function() {
  ## simulate a small dataset with missing outcomes
  set.seed(42)
  n <- 200
  x <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  y <- 1 + a + x + rnorm(n)
  delta <- rbinom(n, 1, lava::expit(1 + x))
  y <- ifelse(delta == 1, y, NA)
  d <- data.frame(y = y, a = a, x = x)

  ## default (no cross-fitting)
  res1 <- moi(
    data = d,
    response.model = learner_glm(y ~ a + x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)"
  )
  expect_true(inherits(res1, "moi.targeted"))
  expect_true(all(is.finite(coef(res1))))

  ## integer nfolds: same partition reused for both internal cate() calls.
  ## Use return.all = TRUE to inspect intermediate components when needed.
  res5 <- moi(
    data = d,
    response.model = learner_glm(y ~ a + x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)",
    nfolds = 5
  )
  expect_true(inherits(res5, "moi.targeted"))
  expect_true(all(is.finite(coef(res5))))

  ## pre-specified list of folds: deterministic partition, reused for both
  ## internal cate() calls.
  custom_folds <- split(seq_len(n), rep(1:4, length.out = n))
  custom_folds <- lapply(custom_folds, sort)
  res_custom <- moi(
    data = d,
    response.model = learner_glm(y ~ a + x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)",
    nfolds = custom_folds
  )
  expect_true(inherits(res_custom, "moi.targeted"))
  expect_true(all(is.finite(coef(res_custom))))
}
test_moi_nfolds()

test_moi_cate_passthrough <- function() {
  ## simulate a small dataset with missing outcomes
  set.seed(42)
  n <- 200
  x <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  y <- 1 + a + x + rnorm(n)
  delta <- rbinom(n, 1, lava::expit(1 + x))
  y <- ifelse(delta == 1, y, NA)
  d <- data.frame(y = y, a = a, x = x)

  ## exercise silent / stratify / second.order forwarding to cate().
  ## mc.cores left at NULL default to keep the test CRAN-friendly.
  ## With stratify = TRUE, response.model and missing.model are fit per
  ## treatment arm; we drop `a` from their RHS to avoid rank-deficient fits.
  res <- suppressWarnings(moi(
    data = d,
    response.model = learner_glm(y ~ x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)",
    nfolds = 3,
    silent = TRUE,
    stratify = TRUE,
    second.order = FALSE
  ))
  expect_true(inherits(res, "moi.targeted"))
  expect_true(all(is.finite(coef(res))))
}
test_moi_cate_passthrough()

test_moi_print_summary <- function() {
  set.seed(13)
  n <- 300
  x <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  y <- 1 + a + x + rnorm(n)
  delta <- rbinom(n, 1, lava::expit(1 + x))
  y <- ifelse(delta == 1, y, NA)
  d <- data.frame(y = y, a = a, x = x)

  res <- moi(
    data = d,
    response.model = learner_glm(y ~ a + x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)"
  )

  ## class structure
  expect_true(inherits(res, "moi.targeted"))
  expect_true(inherits(res, "targeted"))

  ## three coef rows: per-level (a=1, a=0) plus the ATE contrast
  cf <- coef(res)
  expect_equal(length(cf), 3L)
  expect_true(all(is.finite(cf)))

  ## per-arm and contrast labels follow the cate() convention:
  ##   E[\tilde y(a)] for per-arm; [E[\tilde y(1)]] - [E[\tilde y(0)]] for ATE.
  ty <- if (isTRUE(l10n_info()[["UTF-8"]])) "\u1ef9" else "tildeY"
  expect_equal(names(cf)[1], paste0("E[", ty, "(1)]"))
  expect_equal(names(cf)[2], paste0("E[", ty, "(0)]"))
  expect_equal(
    names(cf)[3],
    paste0("[E[", ty, "(1)]] - [E[", ty, "(0)]]")
  )

  ## guard against regression to the obsolete `|A=` label format
  expect_false(any(grepl("|A=", names(cf), fixed = TRUE)))

  ## summary structure
  s <- summary(res)
  expect_true(inherits(s, "summary.moi.targeted"))
  expect_true(all(c("estimate", "call", "ate") %in% names(s)))

  ## printed summary contains "Average Treatment Effect:" header
  out <- capture.output(print(s))
  expect_true(any(grepl("Average Treatment Effect:", out, fixed = TRUE)))
  ## printed summary contains the tilde-y label
  expect_true(any(grepl(ty, out, fixed = TRUE)))
}
test_moi_print_summary()

test_moi_missing_IC <- function() {
  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    y <- 1 + a + x - a * x + w + a * w + z + rnorm(n)           # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(id = n:1, y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }
  set.seed(1)
  data <- simdata(1e2)
  delta <- !is.na(data$y)

  imp_mod <- learner_glm(y ~ a + x,
                         weights = as.numeric(!is.na(data$y)),
                         na.action = lava::na.pass0)
  imp_mod$estimate(data = data)
  pred <- imp_mod$predict(newdata = data, type = "response")
  design_matrix <- imp_mod$design(data = data,
                                    intercept = TRUE,
                                    response = FALSE)$x
  IC_epsilon <- IC(imp_mod$fit)
  family <- family(imp_mod$fit)
  link <- family$link
  family <- family$family
  if (family == "binomial" && link == "logit") {
    nabla <- pred * (1 - pred)
  } else if (family == "gaussian" && link == "identity") {
    nabla <- 1
  } else {
    stop(sprintf("Unsupported family/link combination: family='%s', link='%s'. Supported combinations are: binomial/logit, gaussian/identity", # nolint
                 family, link))
  }
  nabla <- nabla * design_matrix


  A <- data$a
  id <- data$id
  fun <- function(a) {
    newdata <- data

    g <- mean(A == a)
    S <- mean((delta == 1)[A == a])

    ## plug-in estimate
    est <- mean(pred[delta == 0 & A == a])

    IC1 <- (A == a) * (delta == 0) /
      (g * (1 - S)) * (pred - est)

    IC2 <- t(colMeans(nabla[A == a & delta == 0, ]) %*%
             t(IC_epsilon))

    IC <- IC1 + IC2

    out <- estimate(coef = est,
                    IC = IC,
                    id = id,
                    labels = paste0("E[u(", a, ")|d=0]"))
    return(out)
  }
  est_ref <- lapply(
    c(1,0),
    FUN = fun
  )
  est_ref <- do.call("merge", est_ref)

  moi_est <- moi_missing(data = data,
                         delta = delta,
                         id = data$id,
                         treatment.model = learner_glm(a ~ 1, family = binomial()),
                         imputation.model = learner_glm(y ~ a + x),
                         imputation.subset = "!is.na(y)",
                         extended.output = TRUE)

  expect_equal(coef(est_ref), coef(moi_est$estimate), tolerance = 1e-14)
  expect_equal(IC(est_ref), IC(moi_est$estimate), tolerance = 1e-14)

}

test_moi_missing_IC()

test_moi_missing_IC_2 <- function() {
  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    prob <- lava::expit(1 + a + x - a * x + w + a * w + z)
    y <- rbinom(n = n, size = 1, prob = prob) # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(id = n:1, y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }
  set.seed(1)
  data <- simdata(1e2)
  delta <- !is.na(data$y)

  imp_mod <- learner_glm(y ~ a + x,
                         family = binomial(),
                         weights = as.numeric(!is.na(data$y)),
                         na.action = lava::na.pass0)
  imp_mod$estimate(data = data)
  pred <- imp_mod$predict(newdata = data, type = "response")
  design_matrix <- imp_mod$design(data = data,
                                    intercept = TRUE,
                                    response = FALSE)$x
  IC_epsilon <- IC(imp_mod$fit)
  family <- family(imp_mod$fit)
  link <- family$link
  family <- family$family
  if (family == "binomial" && link == "logit") {
    nabla <- pred * (1 - pred)
  } else if (family == "gaussian" && link == "identity") {
    nabla <- 1
  } else {
    stop(sprintf("Unsupported family/link combination: family='%s', link='%s'. Supported combinations are: binomial/logit, gaussian/identity", # nolint
                 family, link))
  }
  nabla <- nabla * design_matrix

  A <- data$a
  id <- data$id
  fun <- function(a) {
    newdata <- data

    g <- mean(A == a)
    S <- mean((delta == 1)[A == a])

    ## plug-in estimate
    est <- mean(pred[delta == 0 & A == a])

    IC1 <- (A == a) * (delta == 0) /
      (g * (1 - S)) * (pred - est)

    IC2 <- t(colMeans(nabla[A == a & delta == 0, ]) %*%
             t(IC_epsilon))

    IC <- IC1 + IC2

    out <- estimate(coef = est,
                    IC = IC,
                    id = id,
                    labels = paste0("E[u(", a, ")|d=0]"))
    return(out)
  }
  est_ref <- lapply(
    c(1,0),
    FUN = fun
  )
  est_ref <- do.call("merge", est_ref)

  moi_est <- moi_missing(data = data,
                         delta = delta,
                         id = data$id,
                         treatment.model = learner_glm(a ~ 1, family = binomial()),
                         imputation.model = learner_glm(y ~ a + x, family = binomial()),
                         imputation.subset = "!is.na(y)",
                         extended.output = TRUE)

  expect_equal(coef(est_ref), coef(moi_est$estimate), tolerance = 1e-14)
  expect_equal(IC(est_ref), IC(moi_est$estimate), tolerance = 1e-14)

}

test_moi_missing_IC_2()

test_moi_missing_IC_reference_lava <- function() {

  simdata <- function(n, full = FALSE) {
    w <- rnorm(n) # unmeasured baseline covariate
    x <- rnorm(n) - 0.5 * w # baseline covariate
    a <- rbinom(n, 1, 0.5)    # treatment
    z <- x + a * w^2 + (1-a) * sin(w) + rnorm(n) # post randomization variable
    delta <- rbinom(n = n, size = 1, prob = lava::expit(2 + z)) # non-missingness indicator
    y <- 1 + a + x - a * x + w + a * w + z + rnorm(n)           # outcome
    y <- ifelse(delta == 1, y, NA)
    d <- data.frame(id = n:1, y = y, z = z, a = a, x = x)
    if(full == TRUE) {
      d <- cbind(d, w = w)
    }
    return(d)
  }
  set.seed(1)
  data <- simdata(1e2)
  delta <- !is.na(data$y)

  moi_est <- moi_missing(data = data,
                         delta = delta,
                         id = data$id,
                         treatment.model = learner_glm(a ~ 1, family = binomial()),
                         imputation.model = learner_glm(y ~ a + x),
                         imputation.subset = "!is.na(y)",
                         extended.output = TRUE)

  ## test the imputation model parameter influence function IC using lava
  imp_mod <- glm(y ~ a + x,
                 data = data,
                 weights = as.numeric(!is.na(data$y)),
                 na.action = lava::na.pass0)
  lava_IC_epsilon <- IC(imp_mod)

  expect_true(max(abs(moi_est$IC_epsilon - lava_IC_epsilon)) < 1e-14)

  ## test the imputation model prediction influence function using lava

  ## relies on numerical deriv
  lava_pred_IC <- estimate(imp_mod,
                           function(p, data) {
                             p["(Intercept)"] + p["x"] * data$x + p["a"] * data$a
                           },
                           data = data,
                           average = FALSE) |> IC()

  ## exact deriv
  lava_pred_IC_2 <- estimate(imp_mod,
                           function(p, data) {
                             pred <- p["(Intercept)"] + p["x"] * data$x + p["a"] * data$a
                             structure(pred, grad = cbind(1, data$a, data$x))
                           },
                           data = data,
                           average = FALSE) |> IC()

  ## exact deriv
  lava_pred_IC_3 <- estimate(imp_mod,
                             predict_glm,
                             data = data,
                             average = FALSE) |> IC()

  expect_true(
    max(abs(lava_pred_IC - lava_pred_IC_2)) < 1e-8
  )

  expect_true(
    max(abs(lava_pred_IC_3 - lava_pred_IC_2)) == 0
  )

  est1 <- estimate(imp_mod,
                   predict_glm,
                   data = data,
                   subset = (data$a == 1) & (delta == FALSE),
                   average = TRUE,
                   id = 1:nrow(data))
  est0 <- estimate(imp_mod,
                   predict_glm,
                   data = data,
                   subset = (data$a == 0) & (delta == FALSE),
                   average = TRUE,
                   id = 1:nrow(data))

  expect_true(
    max(abs(
      coef(moi_est$estimate) -
      coef(c(est1,est0)))) == 0
  )

  expect_true(
    max(abs(moi_est$estimate$IC[, 1, drop = FALSE] - IC(est1)[order(data$id), ])) < 1e-14
  )

  expect_true(
    max(abs(moi_est$estimate$IC[, 2, drop = FALSE] - IC(est0)[order(data$id), ])) < 1e-14
  )
}

test_moi_missing_IC_reference_lava()

test_moi_missing_NA_coef <- function() {
  ## Provoke NA coefficients in the imputation model by introducing an
  ## exactly-collinear predictor (duplicate column). The underlying glm.fit
  ## sets the redundant coefficient to NA. The expected behavior
  ## is that `moi_missing()` produces estimates
  ## numerically equivalent to running with the rank-deficient column
  ## removed (since an NA coef is functionally zero).
  set.seed(1)
  n <- 100
  data <- data.frame(
    id = seq_len(n),
    a = rbinom(n, 1, 0.5),
    x = rnorm(n)
  )
  data$x_dup <- data$x  # exact collinearity with `x`
  data$y <- 1 + data$a + data$x + rnorm(n)
  delta <- rbinom(n, 1, lava::expit(1 + data$x))
  data$y <- ifelse(delta == 1, data$y, NA)

  ## sanity: confirm the underlying glm has a NA coef
  imp_fit <- glm(y ~ a + x + x_dup,
                 data = data,
                 weights = as.numeric(!is.na(data$y)),
                 na.action = lava::na.pass0)
  expect_true(any(is.na(coef(imp_fit))))

  ## reference run: full-rank specification (no x_dup)
  res_full_rank <- moi_missing(
    data = data,
    delta = !is.na(data$y),
    id = data$id,
    treatment.model = learner_glm(a ~ 1, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)"
  )

  ## test run: rank-deficient specification (x_dup duplicates x).
  expect_warning(
    res_rank_def <- moi_missing(
      data = data,
      delta = !is.na(data$y),
      id = data$id,
      treatment.model = learner_glm(a ~ 1, family = binomial()),
      imputation.model = learner_glm(y ~ a + x + x_dup),
      imputation.subset = "!is.na(y)"
    )
  )

  ## coefficients and IC should be numerically equivalent across the two runs
  expect_equal(
    coef(res_rank_def$estimate),
    coef(res_full_rank$estimate),
  )
  expect_equal(
    IC(res_rank_def$estimate),
    IC(res_full_rank$estimate)
  )
}
test_moi_missing_NA_coef()
