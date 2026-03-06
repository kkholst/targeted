#' Mean Missing Outcome Imputation (MOI)
#'
#' Estimates the mean of a given parametric imputation model among observations
#' with a missing outcome and a given treatment. Specifically, it provides
#' estimates of \eqn{E[U(X,A,Z,\theta)|A=a, \Delta=0]}, for an imputation model
#' \eqn{U}, where \eqn{X} denotes baseline covariates, \eqn{A} denotes the
#' treatment, \eqn{Z} denotes post randomization covariates, and \eqn{\Delta}
#' denotes a non-missing indicator. Influence function based standard errors are
#' also provided.
#' @param data A data.frame containing the analysis dataset. Data.table and tibble objects will be coerced to
#'   data.frame.
#' @param delta A vector with the non-missing indicator
#' @param treatment.model Learner object
#' @param imputation.model A learner object of class 'learner_glm' used to fit the
#'   imputation model. The learner must specify the outcome variable and model
#'   formula.
#' @param imputation.subset Optional. A character string containing an R expression that
#'   evaluates to a logical vector indicating which rows to use for fitting the
#'   imputation model. The expression is evaluated in the context of 'data'. If
#'   NULL (default), all rows are used.
#' @param imputation.augmentation
#' @param missing.model
#' @param imputation.augmentation.model
#' @return An estimate object containing:
#'   \item{coef}{Estimates for \eqn{E[U|A=1,\Delta=0]} and
#'    \eqn{E[U|A=0,\Delta=0]}}
#'   \item{IC}{Influence curve values for each observation}
#'   \item{id}{Observation identifiers}
moi <- function(data,
                id,
                delta,
                treatment.model,
                imputation.model,
                imputation.subset = NULL,
                imputation.augmentation = FALSE,
                missing.model = NULL,
                imputation.augmentation.model = NULL) {
  ## input checks
  if (!inherits(imputation.model, "learner_glm")) {
    stop("imputation.model must be of inherited class 'learner_glm'")
  }
  if (inherits(data, c("data.table", "tbl_df"))) {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty (0 rows)")
  }
  if (isTRUE(imputation.augmentation)) {
    if (is.null(missing.model)) {
      stop("provide missing model when imputation.augmentation = TRUE")
    }
  }

  ## evaluate imputation.subset expression
  if (!is.null(imputation.subset)) {
    tryCatch({
      model_rows <- eval(parse(text = imputation.subset),
                         envir = data, enclos = parent.frame())
    }, error = function(e) {
      stop(sprintf("Error evaluating 'imputation.subset' expression: %s", e$message))
    })
  } else {
    model_rows <- rep(TRUE, times = nrow(data))
  }
  # validate imputation.subset result
  if (!is.logical(model_rows)) {
    stop("'imputation.subset' expression must evaluate to a logical vector")
  }
  if (length(model_rows) != nrow(data)) {
    stop(sprintf(
      "'imputation.subset' expression length (%d) does not match data rows (%d)",
              length(model_rows), nrow(data)))
  }
  if (any(is.na(model_rows))) {
    stop("'imputation.subset' expression cannot produce NA values")
  }
  if (!any(model_rows)) {
    stop("'imputation.subset' expression excludes all rows (no TRUE values)")
  }

  # validate rows are available
  if (!any(model_rows)) {
    stop("No observations with non-missing outcome in the selected imputation subset. Cannot fit imputation model.") # nolint
  }

  ## fit imputation model
  imputation.model$estimate(data[model_rows, ])

  ## predict from imputation model
  pred <- imputation.model$predict(newdata = data, type = "response")
  design_matrix <- imputation.model$design(data = data,
                                           intercept = TRUE,
                                           response = FALSE)$x

  # getting the influence function/curve
  epsilon <- estimate(imputation.model$fit, id = id[model_rows])
  n_coef <- length(coef(epsilon))
  tmp <- estimate(coef = 0, IC = rep(0, length(id)), id = id)
  epsilon <- merge(epsilon, tmp)
  rm(tmp)
  epsilon <- estimate(epsilon, keep = (1:n_coef))
  IC_epsilon <- IC(epsilon)[order(id), , drop = FALSE] # keep id ordering

  ## calculating the derivate of the imputation function/model
  family <- family(imputation.model$fit)
  link <- family$link
  family <- family$family
  if (family == "binomial" && link == "logit") {
    nabla <- pred * (1 - pred)
  } else if (family == "gaussian" && link == "identity") {
    nabla <- 1
  } else {
    stop(sprintf("Unsupported family/link combination: family='%s', link='%s'. Supported combinations are: binomial/logit, gaussian/identity", # nolint
                 family, link))
  }
  nabla <- nabla * design_matrix

  ## getting the treatment variable and levels:
  A <- treatment.model$response(data)
  levels <- rev(sort(unique(A)))
  treatment_name <- lava:::getoutcome(treatment.model$formula)


  if (isTRUE(imputation.augmentation)) {
    if (!is.null(imputation.augmentation.model)) {
      ## fitting a model for E[U(X,A,Z;\theta)|W, A]
      imputation.augmentation.model <- imputation.augmentation.model$clone()
      imputation.augmentation.model$update("U_")
      if ("U_" %in% colnames(data)) {
        stop("'U_' column not permitted in data")
      }
      data$U_ <- pred
      imputation.augmentation.model$estimate(data)
      data$U_ <- NULL
    }

    ## fitting missing model
    missing.model <- missing.model$clone()
    missing.model$update("delta")
    if ("delta" %in% colnames(data)) {
      stop("'delta' column not permitted in data")
    }
    data$delta <- delta
    missing.model$estimate(data = data)
    data$delta <- NULL
  }

  # getting the estimate for E[U(X,A,Z;\theta)|A = a, \Delta = 0]
  newdata <- data
  fun <- function(a) {
    g <- mean(A == a)
    S <- mean((delta == 1)[A == a])

    ## plug-in estimate
    est <- mean(pred[delta == 0 & A == a])

    if (isTRUE(imputation.augmentation)) {
      newdata[[treatment_name]] <- a
      if (!is.null(imputation.augmentation.model)) {
        H <- imputation.augmentation.model$predict(newdata = newdata,
                                                   type = "response")
      } else {
        H <- imputation.model$predict(newdata = newdata, type = "response")
      }

      SW <- missing.model$predict(newdata = newdata, type = "response")

      aug2 <- (1 - SW) / (1 - S) * (H - est)
      aug <- (g - (A == a)) / g * aug2

      ## augmented estimate
      est <- est + mean(aug)
    }

    IC <- (A == a) * (delta == 0) /
      (g * (1-S)) * (pred - est)

    IC <- IC +
      t(colMeans(nabla[A == a & delta == 0, ]) %*%
        t(IC_epsilon))

    if (isTRUE(imputation.augmentation)) {
      aug2 <- ((A == a) - g) / g * mean(aug2)
      IC <- IC + aug + aug2
    }

    estimate(coef = est,
             IC = IC,
             id = id,
             labels = paste0("E[U|A=", a, ",delta=0]"))
  }

  est <- lapply(
    levels,
    FUN = fun
  )
  est <- do.call("merge", est)

  out <- list(
    estimate = est,
    imputation.model = imputation.model,
    imputation.subset = imputation.subset,
    levels = as.character(levels)
  )

  return(out)
}

moiate <- function(data,
                   response.model,
                   treatment.model,
                   missing.model,
                   imputation.model,
                   imputation.subset = NULL,
                   imputation.augmentation = FALSE,
                   imputation.augmentation.model = NULL,
                   transform = NULL,
                   back.transform = NULL,
                   return.all = FALSE) {
  ## TODO: check that the missing reponse and treatment strata are well defined
  n <- nrow(data)
  id <- 1:nrow(data)
  if (inherits(data, c("data.table", "tbl_df"))) {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (n == 0) {
    stop("'data' cannot be empty (0 rows)")
  }

  if (inherits(response.model, "formula")) {
    response.model <- learner_glm(response.model)
  }
  if (inherits(treatment.model, "formula")) {
    treatment.model <- learner_glm(treatment.model, family = binomial())
  }
  if (inherits(missing.model, "formula")) {
    missing.model <- learner_glm(missing.model, family = binomial())
  }
  if (inherits(imputation.model, "formula")) {
    imputation.model <- learner_glm(imputation.model)
  }

  ## check that the propensity.model is a learner_glm with family = "binomial",
  ## and that the formula RHS is 1, i.e., only an
  ## intercept is included
  if (!inherits(treatment.model, "learner_glm")) {
    stop("treatment.model must be of inherited class 'learner_glm'")
  }
  ## TODO: implement family S3 function for learner_glm
  family <- treatment.model$.__enclos_env__$private$init$estimate.args$family
  if (inherits(family, "family")) {
    family <- family$family
  }
  if (family != "binomial") {
    stop("treatment.model glm must be of family 'binomial'")
  }
  form <- formula(treatment.model)
  if (length(attr(terms(form), "factors")) != 0) {
    stop("only an intercept is allowed in the treatment.model formula")
  }
  rm(form, family)

  ## clone models that are updated:
  response.model <- response.model$clone()
  missing.model <- missing.model$clone()

  ## extract the non-missing indicator \Delta
  response <- response.model$response(data, na.action = stats::na.pass)
  if (is.null(response)) {
    stop("invalid outcome in response.model")
  }
  delta <- !is.na(response)

  # fit model for E[\Delta Y | A = a]
  response.model$update("delta_response")
  if ("delta_reponse" %in% colnames(data)) {
    stop("'delta_response' column not permitted in data")
  }
  data$delta_response <- ifelse(!delta, 0, response)
  outcome_est <- cate(
    cate.model =  ~ 1,
    response.model = response.model,
    propensity.model = treatment.model,
    data = data
  )
  data$delta_response <- NULL
  outcome_levels <- outcome_est$levels

  # get the influence function/curve
  outcome_est <- estimate(outcome_est,
                          keep = c(1, 2),
                          id = id,
                          labels = paste0("E[DY|A=", outcome_levels, "]"))



  # fit model for P(Delta = 1 | A = a)
  missing.model$update("delta")
  if ("delta" %in% colnames(data)) {
    stop("'delta' column not permitted in data")
  }
  data$delta <- delta
  missing_est <- cate(
    cate.model = ~ 1,
    response.model = missing.model,
    propensity.model = treatment.model,
    data = data
  )
  data$delta <- NULL
  missing_levels <- missing_est$levels

  # calculate P(Delta = 0 | A = a) and get the influence curve/function
  missing_est <- estimate(missing_est, keep = c(1, 2), id = id)
  missing_est <- estimate(missing_est,
                          f = function(x) 1 - x,
                          labels = paste0(
                            "P(D=0|A=", missing_levels, ")"
                          ))

  # fit model for E[U(X,A,Z; theta)|A = a, Delta = 0]
  moi_est <- moi(data = data,
                 delta = delta,
                 treatment.model = treatment.model,
                 imputation.model = imputation.model,
                 imputation.subset = imputation.subset,
                 imputation.augmentation = imputation.augmentation,
                 imputation.augmentation.model = imputation.augmentation.model,
                 missing.model = missing.model)
  moi_levels <- moi_est$levels
  moi_est <- moi_est$estimate

  if (!(identical(missing_levels, outcome_levels) & identical(missing_levels, moi_levels))) {
    stop("treatment levels are not identical")
  }

  ##  output
  est <- merge(outcome_est, missing_est, moi_est)
  ate <- estimate(est,
                  f = function(x) x[1:2] + x[3:4] * x[5:6],
                  labels = paste0("E[tildeY|A=", missing_levels, "]"))
  ate <- estimate(ate, f = cbind(1, -1), labels = "ATE")
  ## transform and back transform
  ate <- estimate(ate, f = transform, back.transform = back.transform)

  if (return.all == TRUE) {
    ate <- merge(est, ate)
  }

  return(ate)
}
