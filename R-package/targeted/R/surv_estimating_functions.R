##' @title Treatment level estimating functions for survival outcomes under right censoring
##' @param type Character string, outcome of interest: "risk": P(T <= tau|A=a), "surv": P(T > tau|A=a)
##' @param data data.frame
##' @param tau Numeric, time-point of interest
##' @param survival_models List of survival models, see fit_survival_models()
##' @param treatment_model Treatment model, see fit_treatment_model()
##' @param control List of control parameters, list(sample, blocksize)
##' @return List with matrix elements estimating_functions, or, and ipw.
##' @author Andreas Nordland
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
    h <- (time <= tau)
    H_constructor <- H_constructor_risk
  } else if (type == "surv") {
    h <- (time > tau)
    H_constructor <- H_constructor_surv
  } else if (type == "rmst") {
    stop("implement")
    ## h: \min(\tilde{T}, \tau)
    h <- pmin(time, tau)

    ## Hu:
    Hu <- Hu_rmst
  } else {
    stop("unknown type. Must be either risk or prob.")
  }

  ## calculating the right censoring augmentation integral:
  rcai <- rcai(
    T_model = T_model,
    C_model = C_model,
    data = data,
    time = time,
    event = event,
    tau = tau,
    H_constructor = H_constructor,
    sample = control$sample,
    blocksize = control$blocksize
  )

  estimating_functions <- matrix(nrow = n, ncol = 2)
  or <- matrix(nrow = n, ncol = 2)
  ipw <- matrix(nrow = n, ncol = 2)
  for (a in 0:1) {
    data_a <- data
    data_a[, A_var] <- A_levels[(a + 1)]

    ## calculating the weight \frac{I\{A_i = a\}}{g(a|X_i)}
    weight <- (A == a) / (a * g1 + (1 - a) * (1 - g1))

    ## calculating E[h_\tau(T)|X, A = a]
    H0 <- H_constructor(
      T_model = T_model,
      tau = tau,
      individual_time = FALSE
    )
    H0 <- H0(u = 0, data = data_a)
    H0 <- as.vector(H0)

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

##' For a user defined function \eqn{H(u|X)}, computes the integral
##' \eqn{\int_0^\tau \frac{H(u)|X}{S^c}} dM^c(u|X), where $S^c$ is the
##' censoring time survival function and $M^c$ is the censoring
##' is the right censoring martingale with the Doob-Meyer decomposition
##' \eqn{M^c = N^c - L^c}, where \eqn{N^c} is the counting process
##' \eqn{N^c(s) = I\{\tilde T \leq s \Delta = 0\}} and \eqn{L^c} is the
##' compensator \eqn{L^c(s) = \int_0^s I \{\tilde T \geq u\} d\Lambda^c(u|X)}.
##' @title Calculate the right censoring augmentation integral
##' @param T_model
##' @param C_model
##' @param data
##' @param time
##' @param event
##' @param tau
##' @param construct_H
##' @param sample
##' @param blocksize
##' @param ...
##' @return
##' @author Andreas Nordland
rcai <- function(T_model,
                 C_model,
                 data,
                 time,
                 event,
                 tau,
                 H_constructor,
                 sample = 0,
                 blocksize = 0,
                 return_all = FALSE,
                 ...) {
  n <- nrow(data)
  data_C <- data[event == 0, , drop = FALSE]
  time_C <- time[event == 0]


  ## Counting term \int_0^\tau \frac{H(u|X)}{S^c} dN^c(u|X):
  H_Nc <- H_constructor(
    T_model = T_model,
    tau = tau,
    individual_time = TRUE
  )
  Sc <- cumhaz(
    C_model,
    newdata = data_C,
    times = time_C,
    individual.time = TRUE
  )$surv
  Nc <- vector(mode = "numeric", length = n)
  Nc[(event == 0)] <- H_Nc(u = time_C, data = data_C) / Sc
  Nc[time > tau] <- 0
  rm(Sc)

  ## Compensator term \int_0^\tau \frac{H(u|X)}{S^c}} d\L^c(u|X):
  H_Lc <- H_constructor(
    T_model = T_model,
    tau = tau,
    individual_time = FALSE
  )

  Lc <- vector(mode = "numeric", length = n)
  tt <- time
  if (sample > 0) {
    tt <- subjumps(time_C, size = sample, tau = tau)
  }

  blocks <- list(1:n)
  if (blocksize > 0) {
    blocks <- lava::csplit(1:n, k = min(n, blocksize))
  }

  for (b in blocks) {
    h_lc <- H_Lc(u = tt, data = data[b, ])
    Sc <- cumhaz(C_model, newdata = data[b, ], times = tt)

    i <- 0
    for (r in b) { ## Loop over each row in the data
      i <- i + 1
      at_risk <- tt <= time[r]
      sc <- Sc$surv[i, ]
      h <- h_lc[i, ]
      dchf <- Sc$dchf[i, ]
      lc <- sum(((h / sc) * at_risk * dchf)[tt <= tau])
      Lc[r] <- lc
    }
  }
  hmc <- Nc - Lc

  if (return_all == TRUE) {
    out <- list(
      Nc = Nc,
      Lc = Lc
    )
  } else {
    out <- hmc
  }

  return(out)
}

## right_censoring_augmentation_integral <- function(T_model,
##                                                   C_model,
##                                                   data,
##                                                   time,
##                                                   event,
##                                                   tau,
##                                                   Hu,
##                                                   sample = 0,
##                                                   blocksize = 0,
##                                                   ...) {
##   n <- nrow(data)
##   data_C <- data[event == 0, , drop = FALSE]
##   time_C <- time[event == 0]

##   ## Counting process term:
##   S <- cumhaz(
##     T_model,
##     newdata = data_C,
##     times = time_C,
##     individual.time = TRUE
##   )$surv
##   S_tau <- cumhaz(
##     T_model,
##     newdata = data_C,
##     times = tau
##   )$surv[, 1]
##   Sc <- cumhaz(
##     C_model,
##     newdata = data_C,
##     times = time_C,
##     individual.time = TRUE
##   )$surv
##   stopifnot(all(S * Sc > 0))
##   Nc <- vector(mode = "numeric", length = n)
##   Nc[(event == 0)] <- Hu(
##     data = data_C,
##     u = time_C,
##     S = S,
##     S_tau = S_tau,
##     tau = tau
##   ) / Sc
##   Nc[time > tau] <- 0
##   rm(S, S_tau, Sc)

##   ## Compensator term:
##   Lc <- vector(mode = "numeric", length = n)
##   tt <- time
##   if (sample > 0) {
##     tt <- subjumps(time_C, size = sample, tau = tau)
##   }

##   blocks <- list(1:n)
##   if (blocksize > 0) {
##     blocks <- lava::csplit(1:n, k = min(n, blocksize))
##   }

##   ii <- 0
##   for (b in blocks) {
##     S <- cumhaz(T_model, newdata = data[b, ], times = tt)$surv
##     S_tau <- cumhaz(T_model, newdata = data[b, ], times = tau)$surv
##     Sc <- cumhaz(C_model, newdata = data[b, ], times = tt)

##     i <- 0
##     for (r in b) { ## Loop over each row in the data
##       i <- i + 1
##       ii <- ii + 1
##       at_risk <- tt <= time[ii]
##       hu <- Hu(
##         data = data[r, ],
##         u = tt,
##         S = S[i, ],
##         S_tau = S_tau[i, ],
##         tau = tau
##       ) / Sc$surv[i, ]
##       lc <- sum((hu * at_risk * Sc$dchf[i, ])[tt <= tau])
##       Lc[r] <- lc
##     }
##   }
##   hmc <- Nc - Lc

##   return(hmc)
## }



