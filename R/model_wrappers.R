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
  A_value <- (A == A_levels[2]) * 1 # binary representation of the treatment variable

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
