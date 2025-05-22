qprob <- function(corr) {
  return(0.5 - mets::pmvn(upper = cbind(0, 0), sigma = corr, cor = TRUE))
}

prob_fct <- function(x, alpha, corr) {
  q <- qprob(corr)
  res <- 0.5 * pchisq(x, 1, lower.tail = FALSE) +
    q * pchisq(x, 2, lower.tail = FALSE) - alpha
  return(res)
}

q_fct <- function(alpha, corr) {
  root <- uniroot(prob_fct,
          alpha = alpha,
          corr = corr,
          interval = c(0, 10)
  )$root
  return(root)
}

###############################################################
# Calculating test statistics
# Calculating p-values
###############################################################
#' @title Signed intersection Wald test
#' @param thetahat1 (numeric) parameter estimate 1
#' @param se1 (numeric) standard error of parameter estimate 1
#' @param thetahat2 (numeric) parameter estimate 2
#' @param se2 (numeric) standard error of parameter estimate 2
#' @param noninf1 (numeric) non-inferiority margin for parameter 1
#' @param noninf2 (numeric) non-inferiority margin for parameter 2
#' @param corr (numeric) correlation between parameter 1 and 2
#' @param alpha (numeric) nominal level
#' @author
#' Christian Bressen Pipper,
#' Klaus Kähler Holst
#' @return list with Wald
test_intersectsignedwald <- function(thetahat1,
                                     se1,
                                     thetahat2,
                                     se2,
                                     noninf1,
                                     noninf2,
                                     corr,
                                     alpha) {
  z1 <- (thetahat1 - noninf1) / se1
  z2 <- (thetahat2 - noninf2) / se2
  zmin <- min(z1, z2)
  zmax <- max(z1, z2)
  SignWald.intersect <- ifelse(zmax >= 0 & zmin <= (corr * zmax), 1, 0) *
    zmax * zmax + ifelse(zmax >= 0 & zmin > (corr * zmax),
      (zmax * zmax + zmin * zmin - 2 * corr * zmax * zmin) / (1 - corr * corr),
      0
    )
  SignWald1 <- ifelse(z1 >= 0, 1, 0) * z1^2
  SignWald2 <- ifelse(z2 >= 0, 1, 0) * z2^2
  critval.intersect <- q_fct(alpha, corr)
  pval.intersect <- ifelse(SignWald.intersect > 0,
    # prob_fct(SignWald.intersect, alpha, corr) + alpha, 1
    prob_fct(SignWald.intersect, 0, corr), 1
    )
  pval1 <- ifelse(SignWald1 > 0,
    0.5 * pchisq(SignWald1, 1, lower.tail = FALSE), 1
  )
  pval2 <- ifelse(SignWald2 > 0,
    0.5 * pchisq(SignWald2, 1, lower.tail = FALSE), 1
  )
  test.int <- structure(list(
    data.name = "H1 ^ H2",
    statistic = c("Q" = unname(SignWald.intersect)),
    parameter = NULL,
    method = "Signed Wald Intersection Test",
    # null.value = "one",
    # alternative = "one.sided",
    p.value = pval.intersect
    # estimate = 1
  ), class = "htest")
  test.1 <- structure(list(
    data.name = sprintf("H1: b1 <= %g", noninf1),
    statistic = c("Q" = unname(SignWald1)),
    estimate = c("b1" = unname(thetahat1)),
    parameter = NULL,
    method = "Signed Wald Test",
    # null.value = noninf1,
    alternative = sprintf("HA1: b1 > %g", noninf1),
    p.value = pval1
  ), class = "htest")
  test.2 <- structure(list(
    data.name = sprintf("H2: b2 <= %g", noninf2),
    statistic = c("Q" = unname(SignWald2)),
    estimate = c("b2" = unname(thetahat2)),
    parameter = NULL,
    method = "Signed Wald Test",
    alternative = sprintf("HA2: b2 > %g", noninf2),
    p.value = pval2
    # estimate = 1
  ), class = "htest")

  res <- list(
    critval.intersect = critval.intersect,
    test.intersect = test.int,
    test.1 = test.1,
    test.2 = test.2
  )
  return(res)

}


#' Estimation of mean clinical outcome truncated by event process
#' @description
#' Let \eqn{Y} denote the clinical outcome, \eqn{A} the binary treatment
#' variable, \eqn{X} baseline covariates, \eqn{T} the failure time,
#' and \eqn{epsilon=1,2} the cause of failure.
#' The following are our two target parameters
#' \deqn{E(Y|T>t, A=1)- E(Y|T>t, A=0)}
#' \deqn{P(T<t,\epsilon=1|A=1)- P(T<t,\epsilon=1|A=0)}
#' @param data (data.frame)
#' @param mod.y (formula or ml_model) Model for clinical outcome given T>time.
#' Using a formula specifies a glm with an identity link (see example).
#' @param mod.r (formula or ml_model) Model for missing data mechanism for
#' clinical outcome at T=time. Using a formula specifies a glm with a log
#' link.
#' @param mod.a (formula or ml_model) Treatment model (in RCT should just be 'a
#' ~ 1'). Using a formula specifies a glm with a log link.
#' @param mod.event (formula) Model for time-to-event process
#' ('Event(time,status) ~ x').
#' @param time (numeric) Landmark time.
#' @param cause (integer) Primary event (in the 'status' variable of the 'Event'
#'   statement).
#' @param cens.code (integer) Censoring code.
#' @param naive (logical) If TRUE, the unadjusted estimates ignoring baseline
#'   covariates is returned as the attribute 'naive'.
#' @param control (list) optimization routine parameters.
#' @param ... Additional arguments passed to [mets::binregATE].
#' @return [lava::estimate.default] object
#' @author Klaus Kähler Holst
#' @examples
#' data(truncatedscore)
#' mod1 <- learner_glm(y ~ a * (x1 + x2))
#' mod2 <- learner_glm(r ~ a * (x1 + x2), family = binomial)
#' a <- estimate_truncatedscore(
#'   data = truncatedscore,
#'   mod.y = mod1,
#'   mod.r = mod2,
#'   mod.a = a ~ 1,
#'   mod.event = mets::Event(time, status) ~ x1+x2,
#'   time = 2
#' )
#' s <- summary(a, noninf.t = -0.1)
#' print(s)
#' parameter(s)
#'
#' # the above is equivalent to
#' # a <- estimate_truncatedscore(
#' #   data = truncatedscore,
#' #   mod.y = y ~ a * (x1 + x2),
#' #   mod.r = r ~ a * (x1 + x2),
#' #   mod.a = a ~ 1,
#' #   mod.event = mets::Event(time, status) ~ x1+x2,
#' #   time = 2
#' # )
#' @export
estimate_truncatedscore <- function(
                     data,
                     mod.y,
                     mod.r,
                     mod.a,
                     mod.event,
                     time,
                     cause = NULL,
                     cens.code = 0,
                     naive = FALSE,
                     control=list(),
                     ...
                     ) {
  if (inherits(mod.y, "formula")) {
    mod.y <- learner_glm(mod.y)
  }
  if (inherits(mod.r, "formula")) {
    mod.r <- learner_glm(mod.r, family = binomial)
  }
  if (inherits(mod.a, "formula")) {
    mod.a <- learner_glm(mod.a, family = binomial)
  }
  # Missing data model
  mod.r$estimate(data)
  r <- mod.r$response(data) == 1
  # Data with Y observed
  d1 <- data[which(r), , drop = TRUE]
  # Outcome model E(Y|R=1,X,A)
  mod.y$estimate(d1)
  # Treatment model
  mod.a$estimate(data)
  a <- mod.a$response(data)
  a0 <- mod.a$response(data, eval = FALSE)
  treatment <- all.vars(update(mod.a$formula, ~1))
  alev <- c(a0[which(a==0)[1]], a0[which(a==1)[1]])
  y <- mod.y$response(data, na.action=lava::na.pass0)
  tmpdata <- data
  est <- ic <- lab0 <- est.naive <- ic.naive <- c()
  for (aval in c(0, 1)) {
    Ia <- (a == aval)
    tmpdata[, treatment] <- alev[aval + 1]
    q1 <- mod.y$predict(tmpdata)
    q2 <- mod.r$predict(tmpdata)
    pa <- mod.a$predict(tmpdata)
    p <- pa[which(a == 1)[1]]
    if (aval == 0) pa <- 1 - pa
    idx1 <- which(Ia)
    pr <- mean(r[idx1]) # P(R=1|A=a)
    m1 <- mean(y[which(Ia & r)]) # E(Y|A=a,R=1)
    est.naive <- c(est.naive, m1)
    ic0 <- r * Ia / (pa * pr) * (y - m1)
    ic.naive <- cbind(ic.naive, ic0)
    ic1 <- ic0 -
      (a - p) * (aval - p) / (p * (1 - p) * pr) * (q1 - m1) * q2
    est1 <- m1 + mean(ic1)
    ic <- cbind(ic, ic1 - mean(ic1))
    est <- c(est, est1)
    lab0 <- c(lab0, sprintf("E(Y|T>%.1f,A=%s)", time, aval))
  }
  lab0 <- c(lab0, "diff")
  res <- estimate(
    coef = c(est, est[2] - est[1]),
    IC = cbind(ic, ic[, 2] - ic[, 1]),
    labels = lab0,
    id = rownames(data)
  )
  formula_event <- update(
    mod.event,
    as.formula(paste("~", treatment))
  )
  fac <- attr(terms(mod.event), "factors")
  if (treatment %in% rownames(fac)) {
    idx <- which(fac[treatment, ] == 1)
    if (length(idx) > 0) {
      xx <- colnames(fac)[idx]
      ff <- as.formula(paste(
        ". ~ . -",
        paste(xx, collapse = "-")
      ))
      mod.event <- update(mod.event, ff)
    }
  }
  if (is.null(control$riskmethod)) {
    control$riskmethod <- "ate_riskRegression"
  }
  b <- do.call(control$riskmethod,
          list(
            mod.marg = formula_event,
            mod.covar = mod.event,
            data = data,
            time = time,
            cause = cause,
            cens.code = cens.code
          ))
  b <- estimate(
    b,
    function(p) c(1 - p[1], 1 - p[2], -p[3]),
    id = rownames(data)
  )
  if (!is.null(cause)) {
    lab <- paste0(gsub(
      "^p", sprintf("P\\(T>%.1f|cause=%s,A=", time, cause),
      names(coef(b))[1:2]
    ), ")")
  } else {
    lab <- paste0(gsub(
      "^p", sprintf("P\\(T>%.1f|A=", time),
      names(coef(b))[1:2]
    ), ")")
  }
  lab <- c(lab, "riskdiff")
  b <- estimate(b, labels = lab)
  res <- merge(res, b)
  if (naive) {
    if (is.null(cause)) {
      b0 <- ate_phreg(
        formula_event,
        data = data, time = time, cens.code = cens.code
      )
    } else {
      b0 <- ate_cmprsk(
        formula_event,
        data = data, time = time, cens.code = cens.code,
        cause = cause
      )
    }
    best0 <- estimate(b0, function(p) {
      return(c(1 - p[1], 1 - p[2], -p[3]))
    }, labels = lab)
    best0$vcov <- vcov(b0)
    res0 <- estimate(
      coef = c(est.naive, est.naive[2] - est.naive[1]),
      IC = cbind(ic.naive, ic.naive[, 2] - ic.naive[, 1]),
      labels = lab0,
      id = seq_len(NROW(ic.naive))
    )
    res0 <- merge(res0, best0)
    res0$model.index <- list(1:3, 4:6)
    res <- structure(res, naive = list(res0))
  }
  res$landmark.time <- time
  class(res) <- c("truncatedscore", class(res))
  return(res)
}

#' @export
summary.truncatedscore <- function(object,
                                   noninf.y = 0,
                                   noninf.t = 0,
                                   alpha = 0.05,
                                   parameter.sign = c(y = 1L, t = 1L),
                                   ...) {

  idx <- if (sign(parameter.sign[1]) < 0) c(1, 2) else c(2, 1)
  idx <- c(idx, if (sign(parameter.sign[2]) < 0) c(4, 5) else c(5, 4))
  nn <- names(coef(object))
  lab <- c(paste0(nn[idx[1]], " - ", nn[idx[2]]),
    paste0(nn[idx[3]], " - ", nn[idx[4]])
  )
  B <- matrix(0, 2, 6)
  B[1, idx[1:2]] <- c(1, -1)
  B[2, idx[3:4]] <- c(1, -1)
  est <- estimate(object, B)
  args <- list(
    thetahat1 = coef(est)[1],
    se1 = vcov(est)[1, 1]**.5,
    thetahat2 = coef(est)[2],
    se2 = vcov(est)[2, 2]**.5,
    corr = cov2cor(vcov(est))[1, 2],
    noninf1 = noninf.y,
    noninf2 = noninf.t,
    alpha = alpha
  )
  pval <- do.call(test_intersectsignedwald, args)
  res1 <- with(pval$test.1, cbind(estimate, statistic, p.value))
  rownames(res1) <- names(pval$test.1$estimate)
  res2 <- with(pval$test.2, cbind(estimate, statistic, p.value))
  rownames(res2) <- names(pval$test.2$estimate)
  res12 <- with(pval$test.intersect, cbind(NA, statistic, p.value))
  rownames(res12) <- "intersection"
  tests <- rbind(res1, res2, res12)
  res <- c(list(
    object = object,
    alpha = alpha,
    estimate = est,
    nonfinf.y = noninf.y,
    noninf.t = noninf.t,
    labels = lab,
    tests = tests
  ), pval)
  class(res) <- "summary.truncatedscore"
  return(res)
}

#' @export
print.summary.truncatedscore <- function(x, ...) {
  cat("\n")
  cli::cli_h2("Parameter estimates")
  print(x$object)
  cat("\n")
  cli::cli_h2("One-sided tests")
  cat("\nb1 = ", x$labels[1], "\n", sep = "")
  print(x$test.1)
  cat("\nb2 = ", x$labels[2], "\n", sep = "")
  print(x$test.2)
  cli::cli_h2("Intersection test")
  print(x$test.intersect)
}

#' @export
parameter.summary.truncatedscore <- function(x, ...) {
  return(x$tests)
}
#' @export
coef.summary.truncatedscore <- function(object, ...) {
  return(object$tests)
}

ate_riskRegression <- function(mod.marg,
                               mod.covar = ~1,
                               data,
                               time,
                               cens.code = 0, ...) {
  trt.var <- tail(all.vars(mod.marg), 1)
  y <- design(mod.marg, data)$y
  data[["time_"]] <- y[, 1]
  data[["status_"]] <- (y[, 2] != cens.code) * 1
  data[["trt_"]] <- factor(data[[trt.var]])
  mod.trt <- reformulate("1", "trt_")
  treat <- glm(mod.trt,
    data = data,
    family = binomial(link = "logit")
  )
  formula_outcome <- update(
    mod.covar,
    survival::Surv(time_, status_) ~ trt_:. + strata(trt_)
  )
  fit <- do.call(
    survival::coxph,
    list(formula_outcome, data = data, x = TRUE, y = TRUE)
  )
  cens <- survival::coxph(
                      survival::Surv(time_, !status_) ~ trt_,
                      data = data, x = TRUE, y = TRUE
                    )
  rr <- riskRegression::ate(fit,
                            data = data,
                            treatment = treat,
                            censor = cens, times = time, cause = 1,
                            estimator = "AIPTW", verbose = FALSE
                            )
  rr1 <- estimate(
    coef = rr$meanRisk$estimate,
    IC = Reduce(cbind, rr$iid[[1]]) * nrow(data)
  )
  res <- estimate(rr1, function(p) c(p, diff(p)))
  return(estimate(res, labels = c("p0", "p1", "riskdiff")))
}

ate_cmprsk <- function(mod.marg, data, time, cens.code = 0, cause = 1) {
  trt.var <- tail(all.vars(mod.marg), 1)
  trt <- factor(data[[trt.var]])
  trt <- (trt == levels(trt)[2]) * 1
  y <- design(mod.marg, data)$y
  a <- cmprsk::cuminc(y[, 1], y[, 2],
    trt,
    cencode = cens.code
  ) |> cmprsk::timepoints(time)
  idx0 <- which(rownames(a$est) == paste("0", cause)) # a=0, cause
  idx1 <- which(rownames(a$est) == paste("1", cause)) # a=1, cause
  res <- estimate(
    coef = a$est[c(idx0, idx1)],
    vcov = diag(c(a$var[c(idx0, idx1)])),
  ) |> estimate(function(p) c(p, diff(p)),
    labels = c("p0", "p1", "riskdiff")
  )
  return(res)
}

ate_km <- function(mod.marg, data, time, cens.code = 0) {
  trt.var <- tail(all.vars(mod.marg), 1)
  y <- design(mod.marg, data)$y
  data[["time_"]] <- y[, 1]
  data[["status_"]] <- (y[, 2] != cens.code) * 1
  data[["trt_"]] <- factor(data[[trt.var]])
  km <- survfit(Surv(time_, status_) ~ strata(trt_), data = data) |>
    summary(time = time)
  b2 <- estimate(coef = 1-km$surv, vcov = diag(km$std.err**2))
  res <- b2 + estimate(b2, diff)
  return(estimate(res, labels = c("p0", "p1", "riskdiff")))
}

ate_phreg <- function(mod.marg, data, time, cens.code = 0) {
  trt.var <- tail(all.vars(mod.marg), 1)
  y <- design(mod.marg, data)$y
  data[["time_"]] <- y[, 1]
  data[["status_"]] <- (y[, 2] != cens.code) * 1
  data[["trt_"]] <- factor(data[[trt.var]])
  data[["trt_"]] <- (data[["trt_"]]==levels(data[["trt_"]])[2]) * 1
  data[["id_"]] <- seq_len(nrow(data))
  d1 <- data[which(data$trt_ == 1), ]
  d0 <- data[which(data$trt_ == 0), ]
  fit1 <- mets::phreg(Event(time_, status_) ~ 1, data = d1)
  fit0 <- mets::phreg(Event(time_, status_) ~ 1, data = d0)
  surv <- function(object, time, ...) {
    ic <- mets::IC(object, baseline = TRUE, time = time, ...)
    est <- mets::predictCumhaz(object$cumhaz, new.time = time)[1, 2]
    res <- lava::estimate(NULL, coef = est, IC = ic) |>
      lava::estimate(function(x) 1 - exp(-x), labels = paste0("surv:", time))
    return(res)
  }
  s1 <- estimate(surv(fit1, 2), id=d1$id_)
  s0 <- estimate(surv(fit0, 2), id=d0$id_)
  res <- s0 + s1
  res <- res + estimate(res, diff)
  return(estimate(res, labels = c("p0", "p1", "riskdiff")))
}


ate_mets <- function(mod.marg, mod.covar = ~1, data, time, cens.code = 0,
  cause = NULL) {
  trt.var <- tail(all.vars(mod.marg), 1)
  y <- design(mod.marg, data)$y
  data[["time_"]] <- y[, 1]
  data[["status_"]] <- y[, 2]
  data[["trt_"]] <- factor(data[[trt.var]])
  if (is.null(cause)) {
    data[["status_"]] <- (y[, 2] != cens.code) * 1
    cause <- 1
  }
  b <- mets::binregATE(
    update(mod.covar, mets::Event(time_, status_) ~ trt_ * .),
    data = data,
    time = time,
    cens.model = reformulate("strata(trt_)"),
    outcome = "cif",
    cause = cause,
    cens.code = cens.code
    )
  res <- estimate(coef = b$riskDR, IC = b$riskDR.iid * (nrow(b$riskDR.iid))) |>
    estimate(function(p) c(p, diff(p)),
      labels = c("p0", "p1", "riskdiff")
      )
  return(res)
}
