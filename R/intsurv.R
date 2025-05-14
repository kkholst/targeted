subjumps <- function(jumptimes, tau, size = 100L) {
  jt <- jumptimes[jumptimes <= tau]
  tt <- seq(min(jt), min(max(jt), tau), length.out = size)
  tt <- jt[unique(mets::fast.approx(jt, tt))]
  return(tt)
}

#' @title Integral approximation of a time dependent function.
#' Computes an approximation of \eqn{\int_start^stop S(t) dt}, where
#' \eqn{S(t)} is a survival function, for a selection of start and stop time
#' points.
#'
#' @param times Numeric vector, sorted time points.
#' @param surv Numeric vector, values of a survival function evaluated at time
#'   points given by \code{times}.
#' @param start Numeric vector, start of the integral.
#' @param stop Numeric vector, end of the integral.
#' @param extend (logical) If TRUE, integral is extended beyond the last
#' observed time point
#' @return Numeric vector, value of the integral.
#' @author Andreas Nordland
int_surv <- function(times, surv,
                     start = 0, stop = max(times), extend = FALSE) {
  times <- as.vector(times)
  surv <- as.vector(surv)

  # input checks:
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
    length(start) == 1 || length(start) == dim_start_stop,
    length(stop) == 1 || length(stop) == dim_start_stop
  )

  if (length(start) == 1 && length(stop) > 1) {
    start <- rep(start, times = dim_start_stop)
  }
  if (length(stop) == 1 && length(start) > 1) {
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

    if (start_k >= stop_k) {
      res[k] <- 0
    } else {
      idx <- which(times <= stop_k & times > start_k)

      if (length(idx) > 0) {
        diff_times_k <- diff(c(start_k, times[idx], stop_k))
        surv_k <- c(surv[idx[1] - 1], surv[idx])
      } else {
        idx_last <- data.table::last(which(times <= stop_k))
        diff_times_k <- diff(c(start_k, stop_k))
        surv_k <- surv[idx_last]
      }
      res[k] <- sum(surv_k * diff_times_k)
    }
  }

  return(res)
}
