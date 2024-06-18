
getjumps <- function(time, dy) {
  jumps <- which(dy < 0)
  stop <- NULL
  if (jumps[length(jumps)] < length(dy))
    stop <- time[stop]
  dt <- c(0,time[jumps], stop)
  surv <- surv[jumps]
}


subjumps <- function(jumptimes, tau, size=100L) {
  jt <- jumptimes[jumptimes<=tau]
  tt <- seq(min(jt), min(max(jt),tau), length.out=size)
  tt <- jt[unique(mets::fast.approx(jt, tt))]
  return(tt)
}


intsurv <- function(time, surv, stop = max(time), jumps.only=FALSE) {
  stop <- min(max(as.vector(time)), stop)
  n <- length(time)
  idx <- which(as.vector(time) <= stop)
  time <- as.vector(time)[idx]
  surv <- as.vector(surv)[idx]
  if (jumps.only) {
    jumps <- which(diff(surv) < 0) + 1
    time <- time[jumps]
    surv <- surv[jumps]
  }
  tj <- c(0, time)
  sj <- surv
  dt <- diff(tj)
  res <- numeric(n)
  res[idx] <- rev(cumsum(rev(sj*dt)))

  list(
    t = tj,
    s = sj,
    dt = dt,
    cint = res,
    value = sum(sj*dt)
  )
}


intsurv2 <- function(object, data, time, stop = max(time), sample = 0, blocksize = 0) {
  tau <- min(max(as.vector(time)), stop)
  n <- NROW(data)
  Lc <- vector(mode = "numeric", length = n)
  tt <- time
  if (sample > 0) {
    tt <- subjumps(time, size = sample, tau = tau)
  }
  blocks <- list(1:n)
  if (blocksize > 0) {
    blocks <- lava::csplit(1:n, k = min(n, blocksize))
  }

  res <- numeric(n)
  for (b in blocks) {
    S <- cumhaz(object, newdata = data[b, ], times = tt)$surv
    i <- 0
    for (r in b) { ## Loop over each row in the data
      i <- i + 1
      int <- intsurv(tt, S[i, ], tau)
      res[r] <- 0
    }
  }
  return(res)
}


##' Computes $\int_0^stop S(t|X) dt$
##'
##' @title
##' @param object Survival model object
##' @param data data.frame containing the covariates X
##' @param times Numeric vector, time points that the survival function is evalauted in
##' @param start Numeric, start of the integral
##' @param stop Numeric, end of the integral
##' @param sample
##' @param blocksize
##' @return Numeric vector of length nrow(data).
##' @author Andreas Nordland
intsurv3 <- function(object, data, times, tau = max(times), sample = 0, blocksize = 0) {

  times <- as.vector(times)

  stopifnot(
    all(times > 0)
  )

  tau <- min(max(times), tau)
  n <- nrow(data)

  tt <- times
  if (sample > 0) {
    tt <- subjumps(times, size = sample, tau = tau)
  }

  blocks <- list(1:n)
  if (blocksize > 0) {
    blocks <- lava::csplit(1:n, k = min(n, blocksize))
  }

  res <- numeric(n)
  for (b in blocks) {
    browser()
    S  <- cumhaz(object, newdata = data[b, ], times = tt)$surv
    i <- 0
    for (r in b) { ## Loop over each row in the data
      i <- i + 1
      int <- intsurv(tt, S[i, ], tau)
      res[r] <- 0
    }
  }
  return(res)
}
