## H(u|X_i, A_i) = E[I\{T_i \leq \tau\} | T_i > u, X_i, A_i] = I\{u \leq \tau \} \frac{S(u|X_i, A_i) - S(\tau|X_i, A_i)}{S(u|X_i, A_i)}
H_constructor_risk <- function(T_model, tau, individual_time, ...) {
  force(T_model)
  force(tau)
  force(individual_time)

  H <- function(u, data) {
    S <- cumhaz(
      T_model,
      newdata = data,
      times = u,
      individual.time = individual_time
    )$surv

    S_tau <- cumhaz(
      T_model,
      newdata = data,
      times = tau,
      individual_time = individual_time
    )$surv
    S_tau <- as.vector(S_tau)


    if (individual_time == FALSE) {
      res <- apply(S, 2, function(x) x - S_tau)
      res <- res / S
      indicator <- (u <= tau)
      res <- apply(res, 1, function(x) x * indicator)
      res <- t(res)
    } else {
      res <- (S - S_tau) / S * (u <= tau)
      res <- as.vector(res)
    }
    return(res)
  }
  return(H)
}

## Hu: H(u|X_i, A_i) = E[I\{T_i > \tau\} | T_i \geq u, X_i, A_i] = I\{u \leq \tau \} \frac{S(\tau|X_i, A_i)}{S(u|X_i, A_i)} +  I\{u > \tau \}
H_constructor_surv <- function(T_model, tau, individual_time, ...) {
  force(T_model)
  force(tau)
  force(individual_time)

  H <- function(u, data) {
    S <- cumhaz(
      T_model,
      newdata = data,
      times = u,
      individual.time = individual_time
    )$surv

    S_tau <- cumhaz(
      T_model,
      newdata = data,
      times = tau,
      individual_time = individual_time
    )$surv
    S_tau <- as.vector(S_tau)

    if (individual_time == FALSE) {
      res <- 1 / S
      res <- apply(res, 2, function(x) x * S_tau)
      indicator_1 <- (u <= tau)
      indicator_2 <- (u > tau)
      res <- apply(res, 1, function(x) x * indicator_1 + indicator_2)
      res <- t(res)
    } else {
      res <- S_tau / S * (u <= tau) + (u > tau)
      res <- as.vector(res)
    }

    return(res)
  }
  return(H)
}

## Hu: H_\tau(u|X_i, A_i) = E[\min(T, \tau) | T_i \geq u, X_i, A_i] = u + \frac{1}{S(u|X,A)} \int_u^\tau S(t|X,A) dt
H_constructor_rmst <- function(T_model, time, event, tau, individual_time) {
  force(T_model)
  force(tau)
  force(individual_time)
  force(time)
  force(event)

  H <- function(u, data) {
    S <- cumhaz(
      T_model,
      newdata = data,
      times = u,
      individual.time = individual_time
    )$surv
    times_T <- time[event == 1]
    S_T <- cumhaz(
      T_model,
      newdata = data,
      times = times_T,
      individual.time = FALSE
    )$surv
    if (individual_time == FALSE) {
      int_S <- apply(
        S_T,
        1,
        function(x) {
          int_surv(times = times_T, surv = x, start = u, stop = tau, extend = FALSE)
        },
        simplify = FALSE
      )
      int_S <- do.call(what = "rbind", int_S)
      res <- 1 / S
      res <- res * int_S
      res <- apply(res, 1, function(x) x + pmin(u, tau))
      res <- t(res)
    } else {
      int_S <- numeric(length = length(u))
      for (k in seq_along(u)) {
        int_S[k] <- int_surv(times = times_T, surv = S_T[k, ], start = u[k], stop = tau, extend = FALSE)
      }
      res <- pmin(u, tau) + 1 / S * int_S
    }
    return(res)
  }

  return(H)
}
