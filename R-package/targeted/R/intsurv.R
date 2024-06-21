
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

##' Computes an approximation of $\int_start^\stop S(t) dt$, where $S(t)$ is a survival function, for a selection of start and stop time points.
##'
##' @title Integral approximation of a time dependent function.
##' @param times Numeric vector, sorted time points.
##' @param surv Numeric vector, values of a survival function evaluated at time points given by \code{times}.
##' @param start Numeric vector, start of the integral.
##' @param stop Numeric vector, end of the integral.
##' @param extend
##' @return Numeric vector, value of the integral.
##' @author Andreas Nordland
int_surv <- function(times, surv, start = 0, stop = max(times), extend = FALSE) {
  times <- as.vector(times)
  surv <- as.vector(surv)

  ## input checks:
  stopifnot(
    is.numeric(times),
    !is.unsorted(times),
    !any(is.na(times)),
    all(times > 0),
    is.vector(surv),
    is.numeric(surv),
    !any(is.na(surv)),
    !is.unsorted(rev(surv)),
    length(times) == length(surv),
    all(surv <= 1),
    all(surv >= 0),
    is.numeric(start),
    is.numeric(stop)
  )
  dim_start_stop <- max(length(start), length(stop))
  stopifnot(
    length(start) == 1 | length(start) == dim_start_stop,
    length(stop) == 1 | length(stop) == dim_start_stop
  )

  if (length(start) == 1 & length(stop) > 1) {
    start <- rep(start, times = dim_start_stop)
  }
  if (length(stop) == 1 & length(start) > 1) {
    stop <- rep(stop, times = dim_start_stop)
  }

  ## adding 0 to times and S(0) = 1 to surv:
  times <- c(0, times)
  surv <- c(1, surv)

  ## should the integral not be extended beyond the last observed time point:
  if (extend == FALSE) {
    stop <- pmin(max(times), stop)
  }

  ## looping through each start/stop pair:
  res <- numeric(length = dim_start_stop)
  for (k in seq_along(start)) {
    start_k <- start[k]
    stop_k <- stop[k]

    idx <- which(times <= stop_k & times >= start_k)

    diff_times_k <- diff(c(start_k, times[idx], stop_k))
    surv_k <- c(surv[idx[1] - 1], surv[idx])

    res[k] <- sum(surv_k * diff_times_k)
  }

  return(res)
}


## ##' Computes $\int_{u_i}^{\tau_i} S(t|X_i) dt$
## ##'
## ##' @title Integration of an estimated survival function.
## ##' @param object Survival model object.
## ##' @param data data.frame containing the covariates X.
## ##' @param times Numeric vector, time points that the survival function is evalauted in.
## ##' @param u Numeric or numeric vector, start of the integral.
## ##' @param tau  Numeric or numeric vector, end of the integral.
## ##' @param sample
## ##' @param blocksize
## ##' @param extend
## ##' @return Numeric vector of length \code{nrow(data)}
## ##' @author Andreas Nordland
## integrate_survival <- function(object, data, times, u = 0, tau = max(times), sample = 0, blocksize = 0, extend = FALSE) {
##   times <- as.vector(times)

##   ## input checks
##   stopifnot(
##     all(times > 0),
##     !is.unsorted(times)
##   )
##   dim_u_tau <- max(length(u), length(tau))
##   stopifnot(
##     length(u) == 1 | length(u) == dim_u_tau,
##     length(tau) == 1 | length(tau) == dim_u_tau
##   )

##   if (extend == FALSE) {
##     tau <- pmin(max(times), tau)
##   }

##   n <- nrow(data)
##   tt <- times

##   ## set subjumps in the interval
##   if (sample > 0) {
##     stop("TODO") # jumps within the interval? or just max(tau)
##     tt <- subjumps(times, size = sample, tau = tau)
##   }

##   browser()

##   blocks <- list(1:n)
##   if (blocksize > 0) {
##     blocks <- lava::csplit(1:n, k = min(n, blocksize))
##   }

##   res <- matrix(nrow = n, ncol = dim_u_tau)
##   for (b in blocks) {
##     S <- cumhaz(object, newdata = data[b, ], times = tt)$surv
##     i <- 0
##     for (r in b) { ## Loop over each row in the data
##       i <- i + 1

##       if (u_singleton) {
##         start <- u
##       } else {
##         start <- u[r]
##       }

##       browser()
##       ## set start value


##       int <- int_fun_time_points(times = tt, fun_values = S[i, ], start = u, stop = tau)
##       res[r] <- 0
##     }
##   }


##   return(res)
## }
