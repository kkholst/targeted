
##' @title Predict the cumulative hazard/survival function for a survival model
##' @param object Survival model object: phreg, coxph, rfsrc, ranger
##' @param newdata data.frame
##' @param times numeric vector: Time points at which the survival model is evalauted. Must be a sorted vector with unique elements. If NULL, the time points associated with the survival model is used.
##' @param individual.time logical: If TRUE the survival object is evaluated at different time points for each row in newdata.
##' The number of rows in newdata and the length of times must be the same.
##' @param ... Additional arguments.
##' @return List with elements:
##' \itemize{
##'   \item time: numeric vector
##'   \item chf: cummulative hazard function. If individual.time = FALSE, matrix with dimension (nrow(newdata), length(times)). If individual.time = TRUE, vector of length length(times).
##'   \item surv: survival function, exp(-chf).
##'   \item dchf: t(diff(rbind(0, t(chf))))
##' }
##' @author Klaus K. Holst, Andreas Nordland
cumhaz <- function(object, newdata, times=NULL, individual.time=FALSE, extend = FALSE,...) {
  n <- nrow(newdata)

  ## input check: times
  if (!is.null(times)) {
    stopifnot(
      is.numeric(times),
      length(times) > 0,
      !anyNA(times),
      !is.unsorted(times),
      !any(duplicated(times))
    )
    if (individual.time == TRUE) {
      stopifnot(
        !is.null(times),
        n == length(times)
      )
    }
  }

  if (inherits(object, "phreg")) {
    if (is.null(times)) times <- object$times
    pp <- predict(object,
      newdata = newdata,
      times = times, se = FALSE,
      individual.time = individual.time, ...
    )
    chf <- pp$cumhaz
    if (individual.time == TRUE) {
      chf <- as.vector(chf)
    }
    tt <- pp$times
  } else if (inherits(object, "rfsrc")) {
    pp <- predict(object, newdata = newdata, oob = TRUE, ...)
    chf <- rbind(pp$chf)
    tt <- pp$time.interest
    if (!is.null(times)) {
      idx <- mets::fast.approx(tt, times)
      chf <- chf[, idx, drop = FALSE]
      tt <- times
    }
    if (individual.time) {
      chf <- diag(chf)
    } ## rfsrc unfortunately does not have the immediate possibility to extract individual survival times
  } else if (inherits(object, "ranger")) {
    num.threads <- object$call$num.threads
    pp <- predict(object, type = "response", data = newdata, num.threads = num.threads, ...)
    chf <- rbind(pp$chf)
    tt <- pp$unique.death.times
    if (!is.null(times)) {
      idx <- mets::fast.approx(tt, times)
      chf <- chf[ , idx, drop = FALSE]
      tt <- times
    }
    if (individual.time) {
      chf <- diag(chf)
    }
  } else if (inherits(object, "coxph")) {
    if (inherits(object, "coxph.null")) { # completely stratified model, i.e., no parameters
      formula <- object$formula

      mf <- model.frame(formula, data = newdata)
      strata <- mf[, 2]
      strata <- data.table(strata = strata)

      sf <- survfit(object)
      ssf <- summary(sf, time = times, extend = extend)
      ssf_df <- data.table::data.table(strata = ssf$strata, time = ssf$time, chf = ssf$cumhaz)

      if (individual.time == FALSE) {
        ssf_df_wide <- data.table::dcast(ssf_df, strata ~ time, value.var = "chf")
        tt <- colnames(ssf_df_wide)[-1] |> as.numeric()

        chf <- merge(strata, ssf_df_wide, by = "strata", all.x = TRUE, sort = FALSE)
        chf[, ("strata") := NULL]
        chf <- as.matrix(chf)
      } else {
        tt <- times
        strata[, ("time") := times]
        ssf_df <- unique(ssf_df)
        chf <- merge(strata, ssf_df, by = c("strata", "time"), all.x = TRUE, sort = FALSE)
        chf[, ("strata") := NULL]
        chf[, ("time") := NULL]
        chf <- unlist(chf) |> unname()
      }
    } else {
      pp <- survfit(object, newdata = newdata)
      pp <- summary(pp, time = times)
      if (!is.null(pp$strata)) {
        stop("cumhaz is not implemented for a coxph model with strata.")
      }
      chf <- t(rbind(pp$cumhaz))
      tt <- pp$time
      if (individual.time == TRUE) {
        chf <- diag(chf)
      }
    }
  } else if (inherits(object, "survfit")) {

    call <- object$call
    formula <- call$formula
    strata_indicator <- !is.null(object$strata)
    ssf <- summary(object, time = times, extend = extend)
    ssf_df <- data.table::data.table(strata = ssf$strata, time = ssf$time, chf = ssf$cumhaz)

    if (strata_indicator == FALSE) {
      tt <- ssf$time
      chf <- ssf$cumhaz

      if (individual.time == FALSE) {
        chf <- matrix(rep(chf, times = n), ncol = length(tt), byrow = TRUE)
        colnames(chf) <- tt
      }
    } else {
      if (is.null(formula)) {
        stop("formula not available for the survfit object.")
      }
      mf <- model.frame(formula, data = newdata)[, -1, drop = FALSE]
      strata <- data.table(strata = strata(mf))

      if (individual.time == FALSE) {

        ssf_df_wide <- data.table::dcast(ssf_df, strata ~ time, value.var = "chf")
        tt <- colnames(ssf_df_wide)[-1] |> as.numeric()

        chf <- merge(strata, ssf_df_wide, by = "strata", all.x = TRUE, sort = FALSE)
        chf[, ("strata") := NULL]
        chf <- as.matrix(chf)
      } else {
        tt <- times
        strata[, ("time") := tt]
        chf <- merge(strata, ssf_df, by = c("strata", "time"), all.x = TRUE, sort = FALSE)
        chf[, ("strata") := NULL]
        chf[, ("time") := NULL]
        chf <- unlist(chf) |> unname()
      }
    }
  } else {
    stop("unknown survival model object. Must be either phreg, rfsrc, ranger, or coxph.")
  }

  ## check output format:
  stopifnot(
    is.vector(tt),
    length(tt) > 0,
    is.numeric(tt),
    !anyNA(tt),
    !is.unsorted(tt)
  )
  if (individual.time == TRUE) {
    stopifnot(
      is.vector(chf)
    )
  } else {
    stopifnot(
      is.matrix(chf)
    )
  }

  if (is.matrix(chf)) {
    stopifnot(
      length(tt) == dim(chf)[2],
      nrow(newdata) == dim(chf)[1]
    )
  } else {
    stopifnot(
      length(tt) == length(chf)
    )
  }

  list(
    time = tt,
    chf = chf,
    surv = exp(-chf),
    dchf = t(diff(rbind(0, t(chf))))
  )
}
