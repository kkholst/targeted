##' @title Binary regression models with right censored outcomes
##' @param response Response formula (e.g., Surv(time, event) ~ D + W).
##' @param censoring Censoring formula (e.g., Surv(time, event == 0) ~ D + A +
##'   W)).
##' @param treatment Optional treatment model (ml_model)
##' @param prediction Optional prediction model (ml_model)
##' @param data data.frame.
##' @param newdata Optional data.frame. In this case the uncentered influence
##'   function evalued in 'newdata' is returned with nuisance parameters
##'   obtained from 'data'.
##' @param tau Time-point of interest, see Details.
##' @param type "risk", "treatment", "rmst", "brier"
##' @param M Number of folds in cross-fitting (M=1 is no cross-fitting).
##' @param call.response Model call for the response model (e.g. "mets::phreg").
##' @param args.response Additional arguments to the response model.
##' @param call.censoring Similar to call.response.
##' @param args.censoring Similar to args.response.
##' @param preprocess (optional) Data pre-processing function.
##' @param efficient If FALSE an IPCW estimator is returned
##' @param control See details
##' @param ... Additional arguments to lower level data pre-processing
##'   functions.
##' @return estimate object
##' @details The one-step estimator depends on the calculation of an integral
##'   wrt. the martingale process corresponding to the counting process N(t) =
##'   I(C>min(T,tau)). This can be decomposed into an integral wrt the counting
##'   process, \eqn{dN_c(t)} and the compensator \eqn{d\Lambda_c(t)} where the
##'   latter term can be computational intensive to calculate. Rather than
##'   calculating this integral in all observed time points, we can make a
##'   coarser evaluation which can be controlled by setting
##'   \code{control=(sample=N)}. With \code{N=0} the (computational intensive)
##'   standard evaluation is used.##'
##' @author Klaus K. Holst, Andreas Nordland
##' @export
riskreg_cens <- function(response,
                        censoring,
                        treatment = NULL,
                        prediction = NULL,
                        data,
                        newdata,
                        tau,
                        type="risk",
                        M = 1,
                        call.response = "phreg",
                        args.response = list(),
                        call.censoring = "phreg",
                        args.censoring = list(),
                        preprocess = NULL,
                        efficient = TRUE,
                        control = list(),
                        ...) {
  dots <- list(...)
  cl <- match.call()
  control0 <- control
  control <- list(sample=500, blocksize=0)
  control[names(control0)] <- control0

  surv.response <- get_response(formula = response, data)
  ord <- order(surv.response[, 1])
  if (missing(tau)) tau <- max(surv.response[, 1])
  data <- data[ord, ]
  surv.response <- surv.response[ord, ]
  surv.censoring <- get_response(formula = censoring, data)

  stopifnot(
    attr(surv.response, "type") == "right", # only allows right censoring
    attr(surv.censoring, "type") == "right", # only allows right censoring
    all(surv.response[, 1] == surv.censoring[, 1]), # time must be equal
    # data must be ordered by time and have no missing values:
    all(order(surv.response[, 1]) == (seq_len(nrow(data))))
  )
  rm(surv.response, surv.censoring)

  data[, "_pred"] <- 0
  data[, "_weight"] <- 1
  if (type[1]=="risk") {
    m <- function(time, data) {
      (time<=tau)
    }
    h <- function(data, time, S, S.tau, tau) {
      (S-S.tau)/S * (time<=tau)
    }
  } else if (type[1]=="rmst") {
    m <- function(time, data) {
      pmin(time, tau)
    }
    h <- function(data, time, S, S.tau, tau) {
      I <- intsurv(time, S, tau)$cint
      (as.vector(time)*as.vector(S) + I) / as.vector(S) *(time<=tau)
    }
  } else if (type[1]=="brier") {
    if (is.null(prediction)) {
      stop("Supply prediction method")
    }
  } else { ## treatment
    if (is.null(treatment))
      stop("Specify treatment formula.")
  }

  if (!is.null(treatment)) { ## Potential outcome
    if (inherits(treatment, "formula"))
      treatment <- SL(treatment, family=binomial())
    A <- treatment$response(data)
    A.levels <- sort(unique(A))
    A.var <- all.vars(update(formula(treatment), ~1))
    A.value <- data[which(A)[1], A.var]
    if (length(A.levels)!=2) stop("Expected binary treatment variable (0,1).")
    if (type == "rmst") {
      m <- function(time, data) {
        pmin(time, tau)*data[, "_weight"] +
          data[, "_pred"]*(1 - data[, "_weight"])
      }
      h <- function(data, time, S, S.tau, tau) {
        I <- intsurv(time, S, tau)$cint
        (as.vector(time)*as.vector(S) +
         I)/as.vector(S) * (time<=tau) * as.vector(data[, "_weight"]) +
          as.vector(data[, "_pred"])*(1 - as.vector(data[, "_weight"]))
      }
    } else {
      type <- "treatment"
      m <- function(time, data) {
        (time<=tau)*data[, "_weight"] +
          data[, "_pred"]*(1 - data[, "_weight"])
      }
      h <- function(data, time, S, S.tau, tau) {
        res <- (S-S.tau)/S * as.vector(data[, "_weight"])*(time<=tau) +
          as.vector(data[, "_pred"])*(1 - as.vector(data[, "_weight"]))
        res
      }
    }
  }
  if (!is.null(prediction)) { ## Brier score
    type <- "brier"
    if (is.character(prediction)) {
      prediction <- data[, prediction]
    }
    if (is.numeric(prediction)) {
      data[, "_pred"] <- prediction
    }
    m <- function(time, data) {
      ((time<=tau) - as.vector(data[, "_pred"]))^2
    }
    h <- function(data, time, S, S.tau, tau) {
      (S - S.tau) / S * (1 - 2 * as.vector(data[, "_pred"])) * (time <= tau) +
        as.vector(data[, "_pred"])^2
    }
  }

  fit.phis <- function(train_data, valid_data) {
    ## pre-processing training data
    if (!is.null(preprocess)) {
      train_data <- do.call(
        "preprocess",
        c(list(data = train_data, call = cl), dots)
      )
    }

    ## time-to-event outcome model
    T.args <- c(
      list(formula = response,
           data = train_data),
      args.response
    )
    T.est <- do.call(what = call.response, T.args)

    ## censoring model
    C.args <- c(
      list(formula = censoring,
           data = train_data),
      args.censoring
    )
    C.est <- do.call(what = call.censoring, C.args)

    ## pre-processing validation data
    if (!is.null(preprocess)) {
      valid_data <- do.call(
        "preprocess",
        c(list(data = valid_data, call = cl), dots)
      )
    }

    valid.time <- get_response(formula = response, valid_data)[, 1]
    valid.event <- get_response(formula = response, valid_data)[, 2]

    ## treatment model
    if (!is.null(treatment)) {
      treatment$estimate(train_data)
      A <- treatment$response(valid_data)
      pr.A <- treatment$predict(valid_data)
      valid_data.a <- valid_data
      valid_data.a[, A.var] <- A.value
      if (type=="rmst") {
        rms <- intsurv2(T.est, valid_data.a,
          time = valid.time,
          stop = tau,
          sample = control$sample,
          blocksize = control$blocksize
          )
        valid_data[, "_pred"] <- rms
      } else {
        Fhat <- 1 - as.vector(cumhaz(T.est,
          newdata = valid_data.a,
          times = tau
          )$surv)
        valid_data[, "_pred"] <- Fhat
      }
      rm(valid_data.a)
      valid_data[, "_weight"] <- A/pr.A
    }

    ## prediction model / brier
    if (!is.null(prediction)) {
      if (inherits(prediction, "ml_model")) {
        prediction$estimate(train_data)
        valid_data[, "_pred"] <- prediction$predict(valid_data)
      }
    }

    aug <- 0
    ## augmentation term
    if (efficient) {
      aug <- binreg_augmentation(
        T.est = T.est,
        C.est = C.est,
        data = valid_data,
        time = valid.time,
        event = valid.event,
        tau = tau,
        h = h,
        phreg = (call.response=="phreg")&&(call.censoring=="phreg"),
        sample=control$sample)
    }
    t. <- pmin(valid.time, tau)
    sc <- cumhaz(C.est, newdata = valid_data, times = t.,
                 individual.time=TRUE)$surv
    mf <- m(valid.time, valid_data)
    delta <- valid.event
    delta[valid.time>tau] <- 1
    phi.0 <- cbind(delta * mf / as.vector(sc) + aug)
    return(phi.0)
  }


  if (!missing(newdata)) {
    ph <- fit.phis(train_data = data, valid_data = newdata)
    return(cbind(ph))
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
  IC <- apply(phis, 2, function(x) x - mean(x))
  out <- lava::estimate(NULL, coef = estimates, IC = IC)
  out$estfun <- phis
  attr(out, "folds") <- folds
  return(out)
}


## vector of size n with values \int_0^tau E(Q_u|T_i>=u,X_i)S^c(u|X_i)}^{-1} d
## M_i^c, h = E(Q|T>u=u,X), function(data, u, S, S.tau, tau)
binreg_augmentation <- function(T.est,
                                C.est,
                                data,
                                time,
                                event,
                                tau,
                                h,
                                phreg=TRUE,
                                sample=0,
                                blocksize=0,
                                ...) {
  n <- nrow(data)
  data.C <- data[event == 0, , drop = FALSE]
  time.C <- time[event == 0]
  delta <- event
  delta[time > tau] <- 1

  S <- cumhaz(T.est,
    newdata = data.C,
    times = time.C,
    individual.time = TRUE
    )$surv
  S.tau <- cumhaz(T.est,
    newdata = data.C,
    times = tau
    )$surv[1, ]
  Sc <- cumhaz(C.est,
    newdata = data.C,
    times = time.C,
    individual.time = TRUE
    )$surv
  stopifnot(all(S * Sc> 0))
  ## Counting process term
  Nc <- vector(mode = "numeric", length = n)
  Nc[(event == 0)] <- h(data.C, time.C, S, S.tau, tau) / Sc
  Nc[time > tau] <- 0

  ## Compensator term
  Lc <- vector(mode = "numeric", length = n)
  tt <- time
  if (sample>0) {
    tt <- subjumps(time.C, size=sample, tau=tau)
  }

  blocks <- list(1:n)
  if (blocksize>0)
    blocks <- lava::csplit(1:n, k=min(n, blocksize))

  for (b in blocks) {
    S <- cumhaz(T.est, newdata = data[b, ], times = tt)$surv
    S.tau <- cumhaz(T.est, newdata = data[b, ], times = tau)$surv
    Sc <- cumhaz(C.est, newdata = data[b, ], times = tt)
    i <- 0
    for(r in b) { ## Loop over each row in the data
      i <- i+1
      at.risk <- tt<=time[i]
      ## E[Q|T>=u, X]
      Eq <- h(data[r, ], tt, S[, i], S.tau[, i], tau) / Sc$surv[, i]
      lc <- sum((Eq * at.risk * Sc$dchf[, i]) [tt<=tau])
      Lc[r] <- lc
    }
  }
  hmc <- Nc - Lc
  return(hmc)
}
