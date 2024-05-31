
##' @title Predict the cumulative hazard/survival function for a survival model
##' @param object Survival model object: phreg, coxph, rfsrc, ranger
##' @param newdata data.frame
##' @param times numeric vector: Time points at which the survival model is evalauted. If NULL, the time points associated with the survival model is used.
##' @param individual.time logical: If TRUE the survival object is evaluated at different time points for each row in newdata.
##' The number of rows in newdata and the length of times must be the same.
##' @param ... Additional arguments.
##' @return List with elements:
##' \itemize{
##'   \item time: numeric vector
##'   \item chf: cummulative hazard function. If individual.time = FALSE, matrix with dimension (nrow(newdata), length(times)). If individual.time = TRUE, vector of length length(times).
##'   \item surv: survival function, exp(-chf).
##'   \item dchf: diff(rbind(0, chf))
##' }
##' @author Klaus K. Holst, Andreas Nordland
cumhaz <- function(object, newdata, times=NULL, individual.time=FALSE, ...) {
  if (inherits(object, "phreg")) {
    if (is.null(times)) times <- object$times
    pp <- predict(object,
      newdata = newdata,
      times = times, se = FALSE,
      individual.time = individual.time, ...
    )
    chf <- t(pp$cumhaz)
    tt <- pp$times
  } else if (inherits(object, "rfsrc")) {
    pp <- predict(object, newdata = newdata, oob = TRUE, ...)
    chf <- t(rbind(pp$chf))
    tt <- pp$time.interest
    if (!is.null(times)) {
      idx <- mets::fast.approx(tt, times)
      chf <- chf[idx, , drop = FALSE]
      tt <- times
    }
    if (individual.time) chf <- diag(chf) ## rfsrc unfortunately does not have the immediate possibility to extract individual survival times
  } else if (inherits(object, "ranger")) {
    num.threads <- object$call$num.threads
    pp <- predict(object, type = "response", data = newdata, num.threads = num.threads, ...)
    chf <- t(rbind(pp$chf))
    tt <- pp$unique.death.times
    if (!is.null(times)) {
      idx <- mets::fast.approx(tt, times)
      chf <- chf[idx, , drop = FALSE]
      tt <- times
    }
    if (individual.time) chf <- diag(chf)
  } else if (inherits(object, "coxph")) {
    if (inherits(object, "coxph.null")) {
      formula <- object$formula
      mf <- model.frame(formula, data = newdata)

      stop("")

      pp <- survfit(object)
      pp <- summary(pp, time = times)

      pp_df <- data.frame(strata = pp$strata, time = pp$time, chf = pp$cumhaz, surv = pp$surv)

    } else {
      pp <- survfit(object, newdata = newdata)
      pp <- summary(pp, time = times)
      chf <- rbind(pp$cumhaz)
      tt <- pp$time
      if (individual.time) chf <- diag(chf)
    }
  } else if (inherits(object, "survfit")) {
    stop("cumhaz is not implemented for survfit objects.")
    pp <- summary(object, time = times)

  } else {
    stop("unknown survival model object. Must be either phreg, rfsrc, ranger, or coxph.")
  }
  list(time=tt, chf=chf, surv=exp(-chf), dchf=diff(rbind(0,chf)))
}
