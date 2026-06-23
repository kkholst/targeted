library("tinytest")

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

  moi_est <- targeted:::moi_missing(data = data,
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

  moi_est <- targeted:::moi_missing(data = data,
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

  moi_est <- targeted:::moi_missing(data = data,
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
                             lava::predict_glm,
                             data = data,
                             average = FALSE) |> IC()

  expect_true(
    max(abs(lava_pred_IC - lava_pred_IC_2)) < 1e-8
  )

  expect_true(
    max(abs(lava_pred_IC_3 - lava_pred_IC_2)) == 0
  )

  est1 <- estimate(imp_mod,
                   lava::predict_glm,
                   data = data,
                   subset = (data$a == 1) & (delta == FALSE),
                   average = TRUE,
                   id = 1:nrow(data))
  est0 <- estimate(imp_mod,
                   lava::predict_glm,
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

test_moi_treatment_model_validation <- function() {
  ## Validates the hardened treatment.model input handling in moi():
  ## only base R stats formulas with an intercept-only RHS are accepted.
  set.seed(1)
  n <- 100
  d <- data.frame(
    id = seq_len(n),
    a = rbinom(n, 1, 0.5),
    x = rnorm(n)
  )
  d$y <- 1 + d$a + d$x + rnorm(n)
  delta <- rbinom(n, 1, lava::expit(1 + d$x))
  d$y <- ifelse(delta == 1, d$y, NA)

  args_common <- list(
    data = d,
    response.model = learner_glm(y ~ a + x),
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)"
  )

  ## (1) bare formula a ~ 1 is the canonical accepted form (positive control)
  res_ok <- do.call(
    moi,
    c(args_common, list(treatment.model = a ~ 1))
  )
  expect_true(inherits(res_ok, "moi.targeted"))

  ## (2) learner objects are no longer accepted
  expect_error(
    do.call(
      moi,
      c(args_common,
        list(treatment.model = learner_glm(a ~ 1, family = binomial())))
    ),
    "must be a base R stats formula"
  )

  ## (3) subclassed formulas are rejected (the `identical(class(...))` check)
  f_subclass <- a ~ 1
  class(f_subclass) <- c("Foo", "formula")
  expect_error(
    do.call(
      moi,
      c(args_common, list(treatment.model = f_subclass))
    ),
    "must be a base R stats formula"
  )

  ## (4) character strings are rejected
  expect_error(
    do.call(
      moi,
      c(args_common, list(treatment.model = "a ~ 1"))
    ),
    "must be a base R stats formula"
  )

  ## (5) formulas with predictor variables on the RHS are rejected
  expect_error(
    do.call(
      moi,
      c(args_common, list(treatment.model = a ~ x))
    ),
    "only an intercept"
  )
}
test_moi_treatment_model_validation()

test_moi_missing_weights <- function() {
  ## Verifies the merge logic for user-supplied weights x imputation.subset.
  set.seed(2)
  n <- 100
  d <- data.frame(
    id = seq_len(n),
    a = rbinom(n, 1, 0.5),
    x = rnorm(n)
  )
  d$y <- 1 + d$a + d$x + rnorm(n)
  delta <- rbinom(n, 1, lava::expit(1 + d$x))
  d$y <- ifelse(delta == 1, d$y, NA)

  ## (a) baseline: no user weights, subset only
  res_a <- targeted:::moi_missing(data = d, delta = !is.na(d$y), id = d$id,
                                  treatment.model = learner_glm(a ~ 1, family = binomial()),
                                  imputation.model = learner_glm(y ~ a + x),
                                  imputation.subset = "!is.na(y)")
  expect_true(all(is.finite(coef(res_a$estimate))))

  ## (b) user weights merge: user_w * model_rows reproduces a manual fit
  user_w <- runif(n, 0.5, 1.5)
  res_b <- targeted:::moi_missing(data = d, delta = !is.na(d$y), id = d$id,
                                  treatment.model = learner_glm(a ~ 1, family = binomial()),
                                  imputation.model = learner_glm(y ~ a + x, weights = user_w),
                                  imputation.subset = "!is.na(y)")
  ref_fit <- glm(y ~ a + x, data = d[!is.na(d$y), ],
                 weights = user_w[!is.na(d$y)],
                 na.action = lava::na.pass0)

  expect_equal(unname(coef(res_b$imputation.model$fit)),
               unname(coef(ref_fit)),
               tolerance = 1e-12)

  ## (c) length mismatch is rejected
  expect_error(
    targeted:::moi_missing(
                 data = d, delta = !is.na(d$y), id = d$id,
                 treatment.model = learner_glm(a ~ 1, family = binomial()),
                 imputation.model = learner_glm(y ~ a + x, weights = runif(n - 1)),
                 imputation.subset = "!is.na(y)"
               ),
    "length"
  )

  ## (d) NA in user weights is rejected
  bad_na <- user_w
  bad_na[1] <- NA
  expect_error(
    targeted:::moi_missing(
      data = d, delta = !is.na(d$y), id = d$id,
      treatment.model = learner_glm(a ~ 1, family = binomial()),
      imputation.model = learner_glm(y ~ a + x, weights = bad_na),
      imputation.subset = "!is.na(y)"
    ),
    "must not contain NA"
  )

  ## (e) negative user weights rejected
  bad_neg <- user_w
  bad_neg[1] <- -0.1
  expect_error(
    targeted:::moi_missing(
      data = d, delta = !is.na(d$y), id = d$id,
      treatment.model = learner_glm(a ~ 1, family = binomial()),
      imputation.model = learner_glm(y ~ a + x, weights = bad_neg),
      imputation.subset = "!is.na(y)"
    ),
    "non-negative"
  )
}
test_moi_missing_weights()

test_moi_missing_subset_zeroweight_equivalence <- function() {
  ## Verifies that subsetting and zero-weighting
  ## are equivalent at both the bare-glm level and the learner_glm level.
  ## This underpins moi_missing()'s use of an
  ## `imputation.subset`-derived 0/1 weight vector to fit the imputation
  ## model on a subset of the data.
  set.seed(3)
  n <- 200
  d <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n))
  d$y <- 1 + d$a + d$x + rnorm(n)
  incl <- rep(c(TRUE, FALSE), c(150, 50))

  ## --- Tier 1: bare glm() ---
  fit_subset <- glm(y ~ a + x, data = d[incl, ])
  fit_zerow  <- glm(y ~ a + x, data = d, weights = as.numeric(incl))
  ## coefs match
  expect_equal(unname(coef(fit_subset)),
               unname(coef(fit_zerow)),
               tolerance = 1e-12)
  ## standard errors match (suppress the expected
  ## "observations with zero weight" dispersion warning from summary.glm)
  se_subset <- sqrt(diag(vcov(fit_subset)))
  se_zerow  <- suppressWarnings(sqrt(diag(vcov(fit_zerow))))
  expect_equal(unname(se_subset), unname(se_zerow), tolerance = 1e-10)
  ## excluded rows in the zero-weighted fit contribute zero to the IC
  ic_zerow <- IC(fit_zerow)
  expect_true(all(ic_zerow[!incl, ] == 0))
  ## the implied vcov (crossprod(IC) / n^2) matches across both fits.
  ## Note: lava::IC.glm normalizes by the actual sample size used in the
  ## fit, so per-row IC values differ by the ratio of n's, but the
  ## resulting variance estimate is identical.
  ic_subset <- IC(fit_subset)
  vcov_from_ic_subset <- crossprod(ic_subset) / nrow(ic_subset)^2
  vcov_from_ic_zerow  <- crossprod(ic_zerow)  / nrow(ic_zerow)^2
  expect_equal(unname(diag(vcov_from_ic_subset)),
               unname(diag(vcov_from_ic_zerow)),
               tolerance = 1e-12)

  ## --- Tier 2: learner_glm wrapped in moi_missing-style fit ---
  lr_subset <- learner_glm(y ~ a + x)
  lr_subset$estimate(data = d[incl, ], na.action = lava::na.pass0)
  lr_zerow  <- learner_glm(y ~ a + x)
  lr_zerow$estimate(data = d, weights = as.numeric(incl),
                    na.action = lava::na.pass0)
  expect_equal(unname(coef(lr_subset$fit)),
               unname(coef(lr_zerow$fit)),
               tolerance = 1e-12)
  se_lr_subset <- sqrt(diag(vcov(lr_subset$fit)))
  se_lr_zerow  <- suppressWarnings(sqrt(diag(vcov(lr_zerow$fit))))
  expect_equal(unname(se_lr_subset),
               unname(se_lr_zerow),
               tolerance = 1e-12)
}
test_moi_missing_subset_zeroweight_equivalence()

test_moi_no_missing_anywhere <- function() {
  ## Case B: full data, no missingness anywhere. moi() should short-circuit
  ## to a standard cate() call and produce identical per-level and ATE
  ## estimates.
  set.seed(11)
  n <- 300
  d <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n))
  d$y <- 1 + d$a + d$x + rnorm(n)

  res_moi <- moi(
    data = d,
    response.model = learner_glm(y ~ a + x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)"
  )
  res_cate <- cate(cate.model = ~ 1,
                   response.model = y ~ a + x,
                   treatment.model = a ~ 1,
                   data = d)

  cf_moi <- coef(res_moi)
  cf_cate <- coef(res_cate$estimate)
  expect_true(all(is.finite(cf_moi)))
  ## Per-level  rows match
  expect_equal(unname(cf_moi[1:2]), unname(cf_cate[1:2]),
               tolerance = 1e-12)
  ## ATE row in moi (row 3) equals (Intercept) row in cate (row 3)
  expect_equal(unname(cf_moi[3]), unname(cf_cate[3]),
               tolerance = 1e-12)

  ## IC
  expect_equal(
    IC(res_moi),
    IC(res_cate),
    check.attributes = FALSE
  )
}
test_moi_no_missing_anywhere()

test_moi_no_missing_in_one_arm <- function() {
  ## Case A: level 0 fully observed, level 1 has missingness.
  ## E[ỹ(0)] should equal the cate estimate for level 0
  ## since no imputation is needed
  ## for that level.
  set.seed(12)
  n <- 300
  a <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  y_full <- 1 + a + x + rnorm(n)
  delta <- ifelse(a == 0, 1L, rbinom(n, 1, lava::expit(0.5 + x)))
  y <- ifelse(delta == 1, y_full, NA)
  d <- data.frame(y = y, a = a, x = x)

  res <- moi(
    data = d,
    response.model = learner_glm(y ~ a + x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)"
  )
  cf <- coef(res)
  expect_true(all(is.finite(cf)))
  expect_false(any(is.nan(cf)))

  ## E[ỹ(0)] (row 2) should equal the cate() E[Y(0)] estimate exactly,
  res_cate <- cate(cate.model = ~ 1,
                   response.model = learner_glm(y ~ a + x),
                   treatment.model = a ~ 1,
                   data = within(d, y[is.na(y)] <- 0))
  ## Note: if the response model is not stratified on treatment level,
  ## missing outcomes of Y(1) will effect the estimate E[Y(0)].
  cf_cate <- coef(res_cate$estimate)
  expect_equal(unname(cf[2]), unname(cf_cate[2]), tolerance = 1e-10)
  ## IC
  expect_equal(
    IC(res)[, 2],
    IC(res_cate)[, 2]
  )

}
test_moi_no_missing_in_one_arm()

test_moi_all_missing_in_one_arm <- function() {
  ## Boundary: arm 1 fully missing. moi() should warn and return finite
  ## estimates with E[ỹ(1)] identified solely by the imputation model.
  set.seed(13)
  n <- 300
  a <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  y_full <- 1 + a + x + rnorm(n)
  delta <- ifelse(a == 0, 1L, 0L)
  y <- ifelse(delta == 1, y_full, NA)
  d <- data.frame(y = y, a = a, x = x)

  ## Capture all warnings; assert the moi-specific warning is among them.
  ## The all-missing-in-arm scenario also triggers several upstream
  ## warnings (glm.fit non-convergence, rank-deficient predictions, lava
  ## IC mean-zero checks); we are only interested in moi()'s own.
  ww <- list()
  res <- withCallingHandlers(
    moi(
      data = d,
      response.model = learner_glm(y ~ a + x),
      treatment.model = a ~ 1,
      missing.model = learner_glm(~ a + x, family = binomial()),
      imputation.model = learner_glm(y ~ a + x),
      imputation.subset = "!is.na(y)"
    ),
    warning = function(w) {
      ww[[length(ww) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("All outcomes are missing in arm",
                        unlist(ww), fixed = TRUE)))
  expect_true(all(is.finite(coef(res))))
}
test_moi_all_missing_in_one_arm()

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
  res_full_rank <- targeted:::moi_missing(
    data = data,
    delta = !is.na(data$y),
    id = data$id,
    treatment.model = learner_glm(a ~ 1, family = binomial()),
    imputation.model = learner_glm(y ~ a + x),
    imputation.subset = "!is.na(y)"
  )

  ## test run: rank-deficient specification (x_dup duplicates x).
  expect_warning(
    res_rank_def <- targeted:::moi_missing(
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

test_moi_augmentation <- function() {
  ## Exercises the imputation.augmentation = TRUE path through moi(),
  ## supplying imputation.augmentation.model as both a formula and an
  ## equivalent learner. This is a regression guard for the
  ## formula -> learner_glm conversion in moi(): the two forms must agree.
  set.seed(7)
  n <- 300
  x <- rnorm(n)
  a <- rbinom(n, 1, 0.5)
  z <- x + rnorm(n)
  y <- 1 + a + x + z + rnorm(n)
  delta <- rbinom(n, 1, lava::expit(1 + x))
  y <- ifelse(delta == 1, y, NA)
  d <- data.frame(y = y, a = a, x = x, z = z)

  common <- list(
    data = d,
    response.model = learner_glm(y ~ a + x),
    treatment.model = a ~ 1,
    missing.model = learner_glm(~ a + x, family = binomial()),
    imputation.model = learner_glm(y ~ a + x + z),
    imputation.subset = "!is.na(y)",
    imputation.augmentation = TRUE
  )

  ## (a) augmentation model supplied as a formula
  res_f <- do.call(
    moi,
    c(common, list(imputation.augmentation.model = ~ a + x))
  )
  expect_true(inherits(res_f, "moi.targeted"))
  expect_true(all(is.finite(coef(res_f))))

  ## (b) augmentation model supplied as the equivalent learner object
  res_l <- do.call(
    moi,
    c(common, list(imputation.augmentation.model = learner_glm(~ a + x)))
  )
  expect_true(inherits(res_l, "moi.targeted"))

  ## formula and learner forms must produce identical estimates
  expect_equal(coef(res_f), coef(res_l), tolerance = 1e-12)
}
test_moi_augmentation()
