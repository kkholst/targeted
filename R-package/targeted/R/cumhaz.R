
cumhaz <- function(object, newdata, times=NULL, individual.time=FALSE, ...) {
  if (inherits(object, "phreg")) {
    if (is.null(times)) times <- object$times
    pp <- predict(object, newdata=newdata,
                  times=times, se=FALSE,
                  individual.time=individual.time, ...)
    chf <- t(pp$cumhaz)
    tt <- pp$times
  } else if (inherits(object, "rfsrc")) {
    pp <- predict(object, newdata=newdata, oob=TRUE, ...)
    chf <- t(rbind(pp$chf))
    tt <- pp$time.interest
    if (!is.null(times)) {
      idx <- mets::fast.approx(tt, times)
      chf <- chf[idx,,drop=FALSE]
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
      chf <- chf[idx,,drop=FALSE]
      tt <- times
    }
    if (individual.time) chf <- diag(chf)
  } else if (inherits(object, "coxph")) {
    pp <- survfit(object, newdata=newdata)
    pp <- summary(pp, time=times)
    chf <- rbind(pp$cumhaz)
    tt <- pp$time
    if (individual.time) chf <- diag(chf)
  }
  list(time=tt, chf=chf, surv=exp(-chf), dchf=diff(rbind(0,chf)))
}
