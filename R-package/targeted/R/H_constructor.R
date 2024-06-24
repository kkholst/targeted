## Constructor for H(u):
## H(u|X_i, A_i) = E[I\{T_i \leq \tau\} | T_i > u, X_i, A_i] = I\{u \leq \tau \} \frac{S(u|X_i, A_i) - S(\tau|X_i, A_i)}{S(u|X_i, A_i)}
H_constructor_risk <- function(T_model, tau, individual_time) {
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


## Constructor for H(u):
## Hu: H(u|X_i, A_i) = E[I\{T_i > \tau\} | T_i \geq u, X_i, A_i] = I\{u \leq \tau \} \frac{S(\tau|X_i, A_i)}{S(u|X_i, A_i)} +  I\{u > \tau \}
H_constructor_surv <- function(T_model, tau, individual_time) {
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
