
test_moi <- function() {

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
