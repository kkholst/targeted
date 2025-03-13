#' Estimation of the Average Treatment Effect among Responders
#'
#' @title Responder Average Treatment Effect
#' @param response Response formula (e.g, Y ~ D*A)
#' @param post.treatment Post treatment marker formula (e.g., D ~ W)
#' @param treatment Treatment formula (e.g, A ~ 1)
#' @param data data.frame
#' @param family Exponential family for response (default gaussian)
#' @param M Number of folds in cross-fitting (M=1 is no cross-fitting)
#' @param pr.treatment (optional) Randomization probability of treatment.
#' @param treatment.level Treatment level in binary treatment (default 1)
#' @param SL.args.response Arguments to SuperLearner for the response model
#' @param SL.args.post.treatment Arguments to SuperLearner for the post
#' treatment indicator
#' @param preprocess (optional) Data preprocessing function
#' @param efficient If TRUE, the estimate will be efficient. If FALSE, the
#' estimate will be a simple plug-in estimate.
#' @param ... Additional arguments to lower level functions
#' @return estimate object
#' @author Andreas Nordland, Klaus K. Holst
#' @export
RATE <- function(response, post.treatment, treatment,
                 data, family = gaussian(), M = 5,
                 pr.treatment, treatment.level,
                 SL.args.response = list(
                   family = gaussian(), SL.library = c("SL.mean", "SL.glm")
                 ),
                 SL.args.post.treatment = list(
                   family = binomial(), SL.library = c("SL.mean", "SL.glm")
                 ),
                 preprocess = NULL, efficient = TRUE, ...) {
  dots <- list(...)
  cl <- match.call()

  A <- get_response(treatment, data)
  A.levels <- sort(unique(A))
  if (length(A.levels) != 2) stop("Expected binary treatment variable")
  if (missing(treatment.level)) {
    treatment.level <- A.levels[2]
  }
  control.level <- setdiff(A.levels, treatment.level[1])
  if (missing(pr.treatment)) {
    pr.treatment <- mean(A == treatment.level[1])
  }

  D.levels <- sort(unique(get_response(post.treatment, data)))
  if (all(D.levels != c(0, 1))) {
    stop(
      "Expected binary post treatment variable (0,1)."
    )
  }

  fit.phis <- function(train_data, valid_data) {
    D.args <- c(list(formula = post.treatment), SL.args.post.treatment)
    D.fit <- do.call(SL, D.args)
    Y.args <- c(list(formula = response), SL.args.response)
    Y.fit <- do.call(SL, Y.args)
    if (!is.null(preprocess)) {
      train_data <- do.call(
        "preprocess",
        c(list(data = train_data, call = cl), dots)
      )
    }
    D.est <- D.fit(train_data)
    Y.est <- Y.fit(train_data)

    A <- as.numeric(get_response(treatment, valid_data) == treatment.level[1])
    D <- as.numeric(get_response(post.treatment, valid_data))
    Y <- get_response(response, valid_data)

    if (!is.null(preprocess)) {
      valid_data <- do.call(
        "preprocess",
        c(list(data = valid_data, call = cl), dots)
      )
    }
    valid_data[lava::getoutcome(treatment)] <- treatment.level[1]
    pr.Ya <- predict(Y.est, valid_data)
    pr.Da <- predict(D.est, valid_data)
    valid_data[lava::getoutcome(treatment)] <- control.level
    pr.Y0 <- predict(Y.est, valid_data)

    phi.a <- A / pr.treatment * (Y - pr.Ya) + pr.Ya
    phi.0 <- (1 - A) / (1 - pr.treatment) * (Y - pr.Y0) + pr.Y0

    phi.d <- A / pr.treatment * (D - pr.Da) + pr.Da

    phis <- list(a1 = phi.a, a0 = phi.0, d = phi.d)

    phis <- do.call(cbind, phis)

    return(phis)
  }

  fit.phis.plugin <- function() {
    A <- get_response(treatment, data)
    D <- get_response(post.treatment, data)
    Y <- get_response(response, data)

    phi.1 <- A / pr.treatment * Y
    phi.0 <- (1 - A) / (1 - pr.treatment) * Y
    phi.D <- A / pr.treatment * D
    phis <- list(a1 = phi.1, a0 = phi.0, d = phi.D)
    return(do.call(cbind, phis))
  }

  n <- nrow(data)
  if (efficient == TRUE) {
    if (M < 2) {
      phis <- fit.phis(data, data)
    } else {
      phis <- matrix(nrow = n, ncol = 3)
      folds <- split(sample(1:n, n), rep(1:M, length.out = n))
      folds <- lapply(folds, sort)
      for (f in folds) {
        train_data <- data[-f, ]
        valid_data <- data[f, ]
        ph <- fit.phis(train_data = train_data, valid_data = valid_data)
        phis[f, ] <- ph
        colnames(phis) <- colnames(ph)
      }
    }
  } else {
    phis <- fit.phis.plugin()
  }

  estimates <- apply(phis, 2, mean)
  rate <- (estimates[["a1"]] - estimates[["a0"]]) / estimates[["d"]]

  IC <- apply(phis, 2, function(x) x - mean(x))
  rate.IC <- 1 / estimates[["d"]] * (IC[, "a1"] - IC[, "a0"] - rate * IC[, "d"])

  estimates <- c(estimates, rate = rate)
  IC <- cbind(IC, rate = rate.IC)

  return(lava::estimate(NULL, coef = estimates, IC = IC))
}

#' Estimation of the Average Treatment Effect among Responders for Survival
#' Outcomes
#'
#' Estimation of
#' \deqn{
#' \frac{P(T \leq \tau|A=1) - P(T \leq \tau|A=1)}{E[D|A=1]}
#' }
#' under right censoring based on plug-in estimates of \eqn{P(T \leq \tau|A=a)}
#' and \eqn{E[D|A=1]}.
#'
#' An efficient one-step estimator of \eqn{P(T \leq \tau|A=a)} is constructed
#' using the efficient influence function
#' \deqn{
#' \frac{I\{A=a\}}{P(A = a)} \Big(\frac{\Delta}{S^c_{0}(\tilde T|X)}
#' I\{\tilde T \leq \tau\} + \int_0^\tau
#' \frac{S_0(u|X)-S_0(\tau|X)}{S_0(u|X)S^c_0(u|X)} d M^c_0(u|X)\Big)
#' }
#' \deqn{
#' + \Big(1 - \frac{I\{A=a\}}{P(A = a)}\Big)F_0(\tau|A=a, W)
#' - P(T \leq \tau|A=a).
#' }
#' An efficient one-step estimator of \eqn{E[D|A=1]} is constructed using the
#' efficient influence function
#' \deqn{
#' \frac{A}{P(A = 1)}\left(D-E[D|A=1, W]\right) + E[D|A=1, W] -E[D|A=1].
#' }
#'
#' @title Responder Average Treatment Effect
#' @param response Response formula (e.g., Surv(time, event) ~ D + W).
#' @param censoring Censoring formula (e.g., Surv(time, event == 0) ~ D + A +
#' W)).
#' @param tau Time-point of interest, see Details.
#' @param call.response Model call for the response model (e.g. "mets::phreg").
#' @param args.response Additional arguments to the response model.
#' @param call.censoring Similar to call.response.
#' @param args.censoring Similar to args.response.
#' @author Andreas Nordland, Klaus K. Holst
#' @inherit RATE
#' @export
RATE.surv <- function(response, post.treatment, treatment, censoring,
                      tau,
                      data,
                      M = 5,
                      pr.treatment,
                      call.response,
                      args.response = list(),
                      SL.args.post.treatment = list(
                        family = binomial(),
                        SL.library = c("SL.mean", "SL.glm")
                      ),
                      call.censoring,
                      args.censoring = list(),
                      preprocess = NULL,
                      ...) {
  dots <- list(...)
  cl <- match.call()

  surv.response <- get_response(formula = response, data)
  surv.censoring <- get_response(formula = censoring, data)

  stopifnot(
    attr(surv.response, "type") == "right", # only allows right censoring
    attr(surv.censoring, "type") == "right", # only allows right censoring
    all(surv.response[, 1] == surv.censoring[, 1]), # time must be equal
    all(order(surv.response[, 1]) == (seq_len(nrow(data)))) # data must be
    # ordered by time and have no missing values
  )
  rm(surv.response, surv.censoring)

  A.levels <- sort(unique(get_response(treatment, data)))
  if (all(A.levels != c(0, 1))) stop(
    "Expected binary treatment variable (0,1)."
  )
  if (missing(pr.treatment)) pr.treatment <- NULL

  D.levels <- sort(unique(get_response(post.treatment, data)))
  if (all(D.levels != c(0, 1))) stop(
    "Expected binary post treatment variable (0,1)."
  )

  fit.phis <- function(train_data, valid_data) {
    # pre-processing training data
    if (!is.null(preprocess)) {
      train_data <- do.call(
        "preprocess",
        c(list(data = train_data, call = cl), dots)
      )
    }

    # post treatment model
    D.args <- c(list(formula = post.treatment), SL.args.post.treatment)
    D.fit <- do.call(SL, D.args)
    D.est <- D.fit(train_data)

    # time-to-event outcome model
    T.args <- c(
      list(formula = response, data = train_data),
      args.response
    )
    T.est <- do.call(what = call.response, T.args)

    # censoring model
    C.args <- c(
      list(formula = censoring, data = train_data),
      args.censoring
    )
    C.est <- do.call(what = call.censoring, C.args)

    # pre-processing validation data
    if (!is.null(preprocess)) {
      valid_data <- do.call(
        "preprocess",
        c(list(data = valid_data, call = cl), dots)
      )
    }

    valid.time <- get_response(formula = response, valid_data)[, 1]
    valid.event <- get_response(formula = response, valid_data)[, 2]

    # constructing the one-step estimator
    f.0 <- F.tau(
      T.est = T.est,
      D.est = D.est,
      data = valid_data,
      tau = tau,
      a = 0,
      treatment = treatment,
      post.treatment = post.treatment
    )
    f.1 <- F.tau(
      T.est = T.est,
      D.est = D.est,
      data = valid_data,
      tau = tau,
      a = 1,
      treatment = treatment,
      post.treatment = post.treatment
    )
    hmc <- HMc.tau(
      T.est = T.est,
      C.est = C.est,
      data = valid_data,
      time = valid.time,
      event = valid.event,
      tau = tau
    )
    sc <- diag(cumhaz(C.est, newdata = valid_data, times = valid.time)$surv)

    rm(T.est, C.est)

    A <- as.numeric(get_response(treatment, valid_data))
    if (is.null(pr.treatment)) {
      pr.treatment <- mean(A)
    }

    phi.0 <- (1 - A) / (1 - pr.treatment) *
      (valid.event / sc * (valid.time <= tau) + hmc)
      + (1 - (1 - A) / (1 - pr.treatment)) * f.0
    phi.1 <- A / (pr.treatment) * (valid.event / sc * (valid.time <= tau) + hmc)
      + (1 - A / (pr.treatment)) * f.1

    D <- as.numeric(get_response(post.treatment, valid_data))
    valid_data[lava::getoutcome(treatment)] <- 1
    pr.d <- predict(D.est, valid_data, type = "response")
    phi.d <- A / pr.treatment * (D - pr.d) + pr.d

    phis <- list(a1 = phi.1, a0 = phi.0, d = phi.d)
    phis <- do.call(cbind, phis)

    return(phis)
  }

  folds <- NULL
  n <- nrow(data)
  if (M < 2) {
    phis <- fit.phis(data, data)
  } else {
    phis <- matrix(nrow = n, ncol = 3)
    folds <- split(sample(1:n, n), rep(1:M, length.out = n))
    folds <- lapply(folds, sort)
    for (f in folds) {
      train_data <- data[-f, ]
      valid_data <- data[f, ]
      ph <- fit.phis(train_data = train_data, valid_data = valid_data)
      phis[f, ] <- ph
      colnames(phis) <- colnames(ph)
    }
  }


  estimates <- apply(phis, 2, mean)
  rate <- (estimates[["a1"]] - estimates[["a0"]]) / estimates[["d"]]

  IC <- apply(phis, 2, function(x) x - mean(x))
  rate.IC <- 1 / estimates[["d"]] * (IC[, "a1"] - IC[, "a0"] - rate * IC[, "d"])

  estimates <- c(estimates, rate = rate)
  IC <- cbind(IC, rate = rate.IC)

  out <- lava::estimate(NULL, coef = estimates, IC = IC)
  attr(out, "folds") <- folds

  return(out)
}



F.tau <- function(T.est, D.est, data, tau, a, treatment, post.treatment) {
  data[lava::getoutcome(treatment)] <- a
  pred.D <- predict(D.est, type = "response", data)

  data[lava::getoutcome(post.treatment)] <- 1
  surv.T.D1 <- cumhaz(T.est, newdata = data, times = tau)$surv[, 1]

  data[lava::getoutcome(post.treatment)] <- 0
  surv.T.D0 <- cumhaz(T.est, newdata = data, times = tau)$surv[, 1]

  surv <- pred.D * surv.T.D1 + (1 - pred.D) * surv.T.D0

  f.tau <- 1 - surv

  return(f.tau)
}

# vector of dim 1:n with values \int_0^tau {S(u|X_i) - S(tau|X_i)} / {S(u|X_i)
# S^c(u|X_i)} d M_i^c
HMc.tau <- function(T.est, C.est, data, time, event, tau) {
  n <- nrow(data)

  jump <- (time <= tau)

  data.C <- data[event == 0, ]
  time.C <- time[event == 0]

  S <- diag(cumhaz(T.est, newdata = data.C, times = time.C)$surv)
  S.tau <- cumhaz(T.est, newdata = data.C, times = tau)$surv[ , 1]
  Sc <- diag(cumhaz(C.est, newdata = data.C, times = time.C)$surv)
  stopifnot(all(S * Sc > 0))

  Nc <- vector(mode = "numeric", length = n)
  Nc[(event == 0)] <- (S - S.tau) / (S * Sc)
  Nc[time > tau] <- 0
  rm(S, S.tau, Sc)

  Lc <- vector(mode = "numeric", length = n)
  S <- cumhaz(T.est, newdata = data, times = time)$surv
  S.tau <- cumhaz(T.est, newdata = data, times = tau)$surv
  Sc <- cumhaz(C.est, newdata = data, times = time)
  for (i in 1:n) {
    at.risk <- c(rep(1, i), rep(0, n - i))

    h <- (S[, i] - S.tau[, i]) / (S[, i] * Sc$surv[, i])

    lc <- sum((h * at.risk * Sc$dchf[, i])[jump])
    Lc[i] <- lc
  }

  hmc <- Nc - Lc

  return(hmc)
}
