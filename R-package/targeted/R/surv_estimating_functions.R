 clean_survival_model <- function(model) {
  if (inherits(model, what = "phreg")) {
    model$call <- NULL
  }

  return(model)
}


##' @title Fit survival nuisance models
##' @param data data.frame
##' @param response Response formula (e.g., Surv(time, event) ~ A + W)
##' @param censoring Censoring formula (e.g., Surv(time, event == 0) ~ A + W))
##' @param response_call Model call for the response model (e.g. "mets::phreg")
##' @param response_args Additional arguments passed to the response model
##' @param censoring_call Similar to response_callb
##' @param censoring_args Similar to response_args
##' @return List with elements T_model and C_model
##' @author Andreas Nordland, Klaus K. Holst
fit_survival_models <- function(data,
                                response,
                                censoring,
                                response_call = "phreg",
                                response_args = list(),
                                censoring_call = "phreg",
                                censoring_args = list()) {
  ## response time-to-event (T) model:
  T_args <- c(
    list(
      formula = response,
      data = data
    ),
    response_args
  )
  T_model <- do.call(what = response_call, T_args)
  T_model <- clean_survival_model(T_model)

  ## censoring (C) model:
  C_args <- c(
    list(
      formula = censoring,
      data = data
    ),
    censoring_args
  )
  C_model <- do.call(what = censoring_call, C_args)
  C_model <- clean_survival_model(C_model)


  out <- list(
    response = response,
    T_model = T_model,
    C_model = C_model
  )

  return(out)
}

fit_treatment_model <- function(data,
                                treatment) {
  # if treatment is a formula, the default super learner is applied:
  if (inherits(treatment, "formula")) {
    treatment <- SL(treatment, family = binomial())
  }

  ## check if both levels are observed:
  A <- treatment$response(data)
  A_levels <- sort(unique(A))
  if (length(A_levels) != 2) {
    stop("Expected binary treatment variable.")
  }
  A_var <- all.vars(update(formula(treatment), ~1)) # name of the treatment variable
  A_value <- (A == A_levels[2]) * 1                 # binary representation of the treatment variable

  ## overwriting the treatment variable with the binary representation:
  data[, A_var] <- A_value

  # fitting the treatment model:
  treatment$estimate(data)

  out <- list(
    A_model = treatment,
    A_var = A_var,
    A_levels = A_levels
  )

  return(out)
}


survival_treatment_level_estimating_functions <- function(type = "risk",
                                                          data,
                                                          tau,
                                                          survival_models,
                                                          treatment_model,
                                                          control) {
  ## getting dimensions:
  n <- nrow(data)

  ## getting survival model elements:
  response <- survival_models$response
  T_model <- survival_models$T_model
  C_model <- survival_models$C_model

  ## getting the time for the response T and the event indicator:
  time <- get_response(formula = response, data)[, 1]
  event <- get_response(formula = response, data)[, 2]

  ## checking if (right) censoring occur:
  if (all(event == 1)) {
    stop("censoring does not occur in the data (fold).")
  }

  ## checking if tau is missing
  if (missing(tau)) {
    tau <- max(time)
  }

  ## calculating \Delta(\tau) = I \{C > \min(T, \tau)\}:
  delta <- event
  delta[time > tau] <- 1

  ## calculating S^c(\min(\tilde T, \tau|X, A):
  Sc <- cumhaz(
    object = C_model,
    newdata = data,
    times = pmin(time, tau),
    individual.time = TRUE
  )$surv |> as.vector()

  ## getting the treatment_model elements:
  A_model <- treatment_model$A_model
  A_levels <- treatment_model$A_levels
  A_var <- treatment_model$A_var

  ## getting the binary treatment variable:
  A <- data[, A_var]
  A <- (A == A_levels[2]) * 1

  ## getting the treatment propensity g(1|X):
  g1 <- A_model$predict(data) |> as.vector()

  if (type == "risk") {
    ## h: I\{\tilde T_i \leq tau\}
    ## vector of dimension n
    h <- (time <= tau)

    ## Hu: H(u|X_i, A_i) = E[I\{T_i \leq \tau\} | T_i > u, X_i, A_i] = I\{u \leq \tau \} \frac{S(u|X_i, A_i) - S(\tau|X_i, A_i)}{S(u|X_i, A_i)}
    ## vector of dimension n
    Hu <- function(data, time, S, S_tau, tau) {
      (S - S_tau) / S * (time <= tau) |> as.vector()
    }
  } else if (type == "prob") {
    ## h: I\{\tilde T_i \leq tau\}
    ## vector of dimension n
    h <- (time > tau)

    ## Hu: H(u|X_i, A_i) = E[I\{T_i > \tau\} | T_i \geq u, X_i, A_i] = I\{u \leq \tau \} \frac{S(u|X_i, A_i) - S(\tau|X_i, A_i)}{S(u|X_i, A_i)}
    ## vector of dimension n
    Hu <- function(data, time, S, S_tau, tau) {
      S_tau / S * (time <= tau) + (time > tau) |> as.vector()
    }
  } else {
    stop("unknown type. Must be either risk or prob.")
  }

  ## calculating the right censoring augmentation integral \int_0^tau H(u|X_i, A_i) S^c(u|X_i)}^{-1} dM_i^c
  rcai <- right_censoring_augmentation_integral(
    T_model = T_model,
    C_model = C_model,
    data = data,
    time = time,
    event = event,
    tau = tau,
    Hu = Hu,
    sample = control$sample,
    blocksize = control$blocksize
  )

  estimating_functions <- matrix(nrow = n, ncol = 2)
  or <- matrix(nrow = n, ncol = 2)
  ipw <- matrix(nrow = n, ncol =2)
  for (a in 0:1) {
    data_a <- data
    data_a[, A_var] <- A_levels[(a + 1)]

    ## calculating the weight \frac{I\{A_i = a\}}{g(a|X_i)}
    weight <- (A == a) / (a * g1 + (1 - a) * (1 - g1))

    if (type == "risk") {
      ## calculating H0: E[I\{T_i \leq \tau\} | X_i, A_i = a] = 1-S(\tau|X_i, A_i = a)
      H0 <- 1 - cumhaz(
        object = T_model,
        newdata = data_a,
        times = tau
      )$surv |> as.vector()
    } else if (type == "prob") {
      ## calculating H0: E[I\{T_i \leq \tau\} | X_i, A_i = a] = 1-S(\tau|X_i, A_i = a)
      H0 <- cumhaz(
        object = T_model,
        newdata = data_a,
        times = tau
      )$surv |> as.vector()
    }

    or[, (a + 1)] <- H0
    ipw[, (a + 1)] <- weight * delta / Sc * h
    estimating_functions[, (a + 1)] <- (1 - weight) * H0 + weight * (delta / Sc * h + rcai)
  }

  colnames(estimating_functions) <- A_levels

  out <- list(
    estimating_functions = estimating_functions,
    or = or,
    ipw = ipw
  )

  return(out)
}

## vector of size n with values \int_0^tau H(u|X_i, A_i) S^c(u|X_i)}^{-1} dM_i^c
## Hu:  H(u|X_i, A_i), function(data, time, S, S_tau, tau)
right_censoring_augmentation_integral <- function(T_model,
                                           C_model,
                                           data,
                                           time,
                                           event,
                                           tau,
                                           Hu,
                                           sample = 0,
                                           blocksize = 0,
                                           ...){
  n <- nrow(data)
  data_C <- data[event == 0, , drop = FALSE]
  time_C <- time[event == 0]

  ## Counting process term:
  S <- cumhaz(
    T_model,
    newdata = data_C,
    times = time_C,
    individual.time = TRUE
  )$surv
  S_tau <- cumhaz(
    T_model,
    newdata = data_C,
    times = tau
  )$surv[, 1]
  Sc <- cumhaz(
    C_model,
    newdata = data_C,
    times = time_C,
    individual.time = TRUE
  )$surv
  stopifnot(all(S * Sc > 0))
  Nc <- vector(mode = "numeric", length = n)
  Nc[(event == 0)] <- Hu(
    data = data_C,
    time = time_C,
    S = S,
    S_tau = S_tau,
    tau =tau
  ) / Sc
  Nc[time > tau] <- 0
  rm(S, S_tau, Sc)

  ## Compensator term:
  Lc <- vector(mode = "numeric", length = n)
  tt <- time
  if (sample > 0) {
    tt <- subjumps(time_C, size = sample, tau = tau)
  }

  blocks <- list(1:n)
  if (blocksize > 0) {
    blocks <- lava::csplit(1:n, k = min(n, blocksize))
  }

  ii <- 0
  for (b in blocks) {
    S <- cumhaz(T_model, newdata = data[b, ], times = tt)$surv
    S_tau <- cumhaz(T_model, newdata = data[b, ], times = tau)$surv
    Sc <- cumhaz(C_model, newdata = data[b, ], times = tt)

    i <- 0
    for (r in b) { ## Loop over each row in the data
      i <- i + 1
      ii <- ii + 1
      at_risk <- tt <= time[ii]
      hu <- Hu(
        data = data[r, ],
        time = tt,
        S = S[i, ],
        S_tau = S_tau[i, ],
        tau = tau
      ) / Sc$surv[i, ]
      lc <- sum((hu * at_risk * Sc$dchf[i, ])[tt <= tau])
      Lc[r] <- lc
    }
  }
  hmc <- Nc - Lc

  return(hmc)
}
