## cate() with missing outcomes (IPW via `missing.model`)

library("tinytest")

sim_missing_cate <- function(n = 2000, seed = 1) {
  set.seed(seed)
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  a  <- rbinom(n, 1, plogis(-0.2 + 0.3 * w1))
  y_full <- 1 + a + w1 + 0.5 * w2 + rnorm(n)
  pR <- plogis(0.5 + 0.6 * w1 + 0.5 * a)
  R  <- rbinom(n, 1, pR)
  y  <- ifelse(R == 1, y_full, NA_real_)
  data.frame(y0 = y_full, y = y, a = a, w1 = w1, w2 = w2)
}
d <- sim_missing_cate()

test_cate_missing_null_no_na <- function() {
  ## No NAs and missing.model=NULL (default) reproduces prior behavior:
  ## the IPMW machinery must stay switched off.
  fit <- cate(cate.model = ~1,
            response.model = y0 ~ a * (w1 + w2),
            treatment.model = a ~ w1 + w2,
            data = d)
  expect_true(is.null(fit$missing.model))
  ## `data$pr` is the switch: NULL unless IPMW is active. `data$r` (the
  ## observation indicator) is always stored.
  expect_true(is.null(fit$data$pr))
  expect_equal(as.integer(fit$data$r), rep(1L, nrow(d)))
  expect_true(all(!is.na(coef(fit))))
}
test_cate_missing_null_no_na()

test_cate_missing_nuisance_invariants <- function() {
  ## Guards the `r` (indicator) / `pr` (probability matrix) convention.
  fit <- cate(cate.model = ~1,
            response.model = y ~ a * (w1 + w2),
            treatment.model = a ~ w1 + w2,
            missing.model  = ~ a * (w1 + w2),
            data = d)
  ## r: length-n 0/1 observation indicator matching is.na(y)
  expect_equal(length(fit$data$r), nrow(d))
  expect_true(all(fit$data$r %in% c(0L, 1L)))
  expect_equal(as.integer(fit$data$r), as.integer(!is.na(d$y)))
  ## pr: list (one entry per replication) of n x n_contrast probabilities
  expect_true(is.list(fit$data$pr))
  expect_equal(length(fit$data$pr), 1L)
  expect_equal(dim(fit$data$pr[[1]]), c(nrow(d), 2L))
  expect_true(all(fit$data$pr[[1]] > 0 & fit$data$pr[[1]] < 1))
}
test_cate_missing_nuisance_invariants()

test_cate_missing_null_with_na_errors <- function() {
  expect_error(
    cate(cate.model = ~1,
         response.model = y ~ a * (w1 + w2),
         treatment.model = a ~ w1 + w2,
         data = d),
    pattern = "missing.model"
  )
}
test_cate_missing_null_with_na_errors()

test_cate_missing_unused_message <- function() {
  expect_message(
    cate(cate.model = ~1,
         response.model = y0 ~ a * w1,
         treatment.model = a ~ w1,
         missing.model = ~ a * w1,
         data = d),
    pattern = "no NAs"
  )
}
test_cate_missing_unused_message()

test_cate_missing_ipcw_recovers_ate <- function() {
  fit <- cate(cate.model = ~1,
              response.model = y ~ a * (w1 + w2),
              treatment.model = a ~ w1 + w2,
              missing.model  = ~ a * (w1 + w2),
              data = d)
  b <- coef(fit)
  ## Truth: E[Y(1)] = 2, E[Y(0)] = 1, ATE = 1.
  expect_equal(unname(b["E[y(1)]"]), 2, tolerance = 0.1)
  expect_equal(unname(b["E[y(0)]"]), 1, tolerance = 0.1)
  expect_equal(unname(b["(Intercept)"]), 1, tolerance = 0.1)
  ## Complete-case naive should be visibly biased on the same DGP.
  d_cc <- d[!is.na(d$y), ]
  fit_cc <- cate(cate.model = ~1,
                 response.model = y ~ a * (w1 + w2),
                 treatment.model = a ~ w1 + w2,
                 data = d_cc)
  b_cc <- coef(fit_cc)
  expect_true(abs(b_cc["E[y(1)]"] - 2) > abs(b["E[y(1)]"] - 2))
  expect_true(abs(b_cc["E[y(0)]"] - 1) > abs(b["E[y(0)]"] - 1))
}
test_cate_missing_ipcw_recovers_ate()

test_cate_missing_stratify <- function() {
  fit <- cate(cate.model = ~1,
              response.model = y ~ w1 + w2,
              treatment.model = a ~ w1 + w2,
              missing.model  = ~ w1 + w2,
              stratify = TRUE,
              data = d)
  b <- coef(fit)
  expect_equal(unname(b["E[y(1)]"]), 2, tolerance = 0.1)
  expect_equal(unname(b["E[y(0)]"]), 1, tolerance = 0.1)
  ## the missing model is fit per arm here; the second-order correction is
  ## still applied (see test_cate_missing_stratify_matches_pooled)
  expect_true(all(!is.na(b)))
}
test_cate_missing_stratify()

test_cate_missing_cv <- function() {
  fit <- cate(cate.model = ~1,
              response.model = y ~ a * (w1 + w2),
              treatment.model = a ~ w1 + w2,
              missing.model  = ~ a * (w1 + w2),
              nfolds = 3, silent = TRUE,
              data = d)
  b <- coef(fit)
  expect_equal(unname(b["(Intercept)"]), 1, tolerance = 0.15)
  expect_true(all(!is.na(b)))
}
test_cate_missing_cv()

test_cate_missing_calibration <- function() {
  r <- cate(cate.model = ~1,
            response.model = y ~ a * (w1 + w2),
            treatment.model = a ~ w1 + w2,
            missing.model  = ~ a * (w1 + w2),
            calibration.model = ~ w1 + w2,
            data = d)
  cf <- coef(r)
  expect_equal(unname(cf["(Intercept)"]), 1, tolerance = 0.15)
  expect_true(all(!is.na(cf)))
}
test_cate_missing_calibration()

test_cate_missing_var_type_fallback <- function() {
  expect_warning(
    cate(cate.model = ~1,
         response.model = y ~ a * (w1 + w2),
         treatment.model = a ~ w1 + w2,
         missing.model  = ~ a * (w1 + w2),
         calibration.model = ~ w1 + w2,
         var.type = "adaptive",
         data = d),
    pattern = "IC"
  )
}
test_cate_missing_var_type_fallback()

test_cate_missing_r_column_conflict <- function() {
  d$R_ <- 1L
  expect_error(
    cate(cate.model = ~1,
         response.model = y ~ a * (w1 + w2),
         treatment.model = a ~ w1 + w2,
         missing.model  = ~ a * (w1 + w2),
         data = d),
    pattern = "R_"
  )
}
test_cate_missing_r_column_conflict()

test_cate_missing_learner_object <- function() {
  ## Supplying missing.model as a pre-built learner (not a formula).
  mmod <- learner_glm(R_ ~ a * (w1 + w2), family = binomial())
  r <- cate(cate.model = ~1,
            response.model = y ~ a * (w1 + w2),
            treatment.model = a ~ w1 + w2,
            missing.model  = mmod,
            data = d)
  expect_true(all(!is.na(coef(r))))
  expect_true(!is.null(r$data$pr))
}
test_cate_missing_learner_object()

test_cate_missing_rep <- function() {
  ## rep > 1 stores one `pr` matrix per replication; exercises the
  ## per-replication indexing in update.cate.targeted().
  r <- cate(cate.model = ~1,
            response.model = y ~ a * (w1 + w2),
            treatment.model = a ~ w1 + w2,
            missing.model  = ~ a * (w1 + w2),
            nfolds = 2, rep = 2, silent = TRUE,
            data = d)
  expect_equal(length(r$data$pr), 2L)
  expect_equal(length(r$data$q), 2L)
  expect_true(all(vapply(r$data$pr, \(x) all(dim(x) == c(nrow(d), 2L)), TRUE)))
  expect_equal(unname(coef(r)["(Intercept)"]), 1, tolerance = 0.15)
}
test_cate_missing_rep()

test_cate_missing_second_order <- function() {
  ## second.order toggles the missing-model correction in the influence
  ## function: point estimates identical, standard errors must differ.
  args <- list(cate.model = ~1,
               response.model = y ~ a * (w1 + w2),
               treatment.model = a ~ w1 + w2,
               missing.model  = ~ a * (w1 + w2),
               data = d)
  m1 <- do.call(cate, args)
  m0 <- do.call(cate, c(args, list(second.order = FALSE)))
  expect_equal(coef(m1), coef(m0))
  expect_false(isTRUE(all.equal(vcov(m1$estimate), vcov(m0$estimate))))
}
test_cate_missing_second_order()

test_cate_missing_double_robust <- function() {
  ## Misspecified outcome model, correct treatment + missingness models:
  ## the ATE should still be approximately unbiased.
  r <- cate(cate.model = ~1,
            response.model = y ~ a,
            treatment.model = a ~ w1 + w2,
            missing.model  = ~ a * (w1 + w2),
            data = d)
  expect_equal(unname(coef(r)["(Intercept)"]), 1, tolerance = 0.15)
}
test_cate_missing_double_robust()
