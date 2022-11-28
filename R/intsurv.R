

## tt <- c(0,d$time)
## ss <- c(1,s1(d$time))
## val <- intsurv(tt, ss, 1)

intsurv <- function(time, surv, stop = max(time)) {
  stop <- min(max(time), stop)
  idx <- which(time <= stop)
  time <- time[idx]
  surv <- surv[idx]
  jumps <- which(diff(surv) < 0) + 1
  tj <- c(0, time[jumps])
  sj <- c(1, surv[jumps])
  dt <- diff(c(tj, stop))
  list(
    t = tj,
    s = sj,
    dt = dt,
    I = sum(sj * dt)
  )
}
