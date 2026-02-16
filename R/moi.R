#' Mean Missing Outcome Imputation (MOI)
#'
#' Estimates the mean of a given parametric imputation model among observations
#' with a missing outcome and a given treatment. Specifically, it provides
#' estimates of \eqn{E[U(X,A,Z,\theta)|A=a, \Delta=0]}, for an imputation model \eqn{U},
#' where \eqn{X} denotes baseline covariates,
#' \eqn{A} denotes the treatment, \eqn{Z} denotes post randomization covariates, and \eqn{\Delta}
#' denotes a non-missing indicator. Influence function based standard errors are
#' also provided.
#' @param data A data.frame containing the analysis dataset. Must include columns
#'   'id' (unique identifier) and 'a' (binary treatment variable with values 0 or 1).
#'   Data.table and tibble objects will be coerced to data.frame.
#' @param learner A learner object of class 'learner_glm' used to fit the imputation
#'   model. The learner must specify the outcome variable and model formula.
#' @param subset Optional. A character string containing an R expression that
#'   evaluates to a logical vector indicating which rows to use for fitting the
#'   imputation model. The expression is evaluated in the context of 'data'.
#'   If NULL (default), all rows with non-missing outcomes are used.
#'
#' @return An estimate object containing:
#'   \item{coef}{Estimates for \eqn{E[U|A=1,\Delta=0]} and \eqn{E[U|A=0,\Delta=0]}}
#'   \item{IC}{Influence curve values for each observation}
#'   \item{id}{Observation identifiers}
moi <- function(data,
                learner,
                subset = NULL) {


  ## input checks
  if (!inherits(learner, "learner_glm")) {
    stop("imputation learner must be of inherited class 'learner_glm'")
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
  required_cols <- c("id", "a")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("'data' is missing required column(s): %s",
                 paste(missing_cols, collapse = ", ")))
  }
  rm(required_cols, missing_cols)
  if (any(is.na(data[["id"]]))) {
    stop("'id' column cannot contain missing values (NA)")
  }
  if (any(duplicated(data[["id"]]))) {
    stop("'id' column contains duplicate values")
  }
  if (any(is.na(data[["a"]]))) {
    stop("'a' column cannot contain missing values (NA)")
  }
  unique_a <- unique(data[["a"]])
  if (!all(unique_a %in% c(0, 1))) {
    stop("'a' must be binary (contain only 0 and 1 values)")
  }

  ## evaluate subset expression
  if (!is.null(subset)) {
    tryCatch({
      model_rows <- eval(parse(text = subset), envir = data, enclos = parent.frame())
    }, error = function(e) {
      stop(sprintf("Error evaluating 'subset' expression: %s", e$message))
    })
  } else {
    model_rows <- rep(TRUE, times = nrow(data))
  }
  # validate subset result
  if (!is.logical(model_rows)) {
    stop("'subset' expression must evaluate to a logical vector")
  }
  if (length(model_rows) != nrow(data)) {
    stop(sprintf("'subset' expression length (%d) does not match data rows (%d)",
                 length(model_rows), nrow(data)))
  }
  if (any(is.na(model_rows))) {
    stop("'subset' expression cannot produce NA values")
  }
  if (!any(model_rows)) {
    stop("'subset' expression excludes all rows (no TRUE values)")
  }

  ## extracting the non-missing indicator
  delta <- !is.na(learner$response(data, na.action = stats::na.pass))

  ## combining model rows with delta == 1
  model_rows <- model_rows & (delta == 1)

  # validate rows are available
  if (!any(model_rows)) {
    stop("No observations with non-missing outcome in the selected subset. Cannot fit imputation model.")
  }

  # check potential issues with stratification on treatment
  if (!any(delta == 0 & data[["a"]] == 0)) {
    warning("No observations with missing outcomes and a == 0. Estimate for E[U|A=0,delta=0] is invalid.")
  }
  if (!any(delta == 0 & data[["a"]] == 1)) {
    warning("No observations with missing outcomes and a == 1. Estimate for E[U|A=1,delta=0] is invalid.")
  }

  ## extract id
  id <- data[["id"]]

  ## fit imputation model
  learner$estimate(data[model_rows, ])

  ## predict from imputation model
  pred <- learner$predict(newdata = data, type = "response")
  design_matrix <- learner$design(data = data,
                                  intercept = TRUE,
                                  response = FALSE)$x

  # getting the influence function/curve
  epsilon <- estimate(learner$fit, id = id[model_rows])
  n_coef <- length(coef(epsilon))
  tmp <- estimate(coef = 0, IC = rep(0, length(id)), id = id)
  epsilon <- merge(epsilon, tmp)
  rm(tmp)
  epsilon <- estimate(epsilon, keep = (1:n_coef))

  ## calculating the derivate of the imputation function/model
  family <- learner$fit$family$family
  link <- learner$fit$family$link
  if (family == "binomial" && link == "logit") {
    nabla <- pred * (1 - pred)
  } else if (family == "gaussian" && link == "identity") {
    nabla <- 1
  } else {
    stop(sprintf("Unsupported family/link combination: family='%s', link='%s'. Supported combinations are: binomial/logit, gaussian/identity",
                 family, link))
  }
  nabla <- nabla * design_matrix

  # getting the estimate for E[U(X,A;\theta)|A = a, \Delta = 0]
  fun <- function(a) {
    est <- mean(pred[delta == 0 & data[["a"]] == a])

    IC <- (data[["a"]] == a) * (delta == 0) /
      mean((data[["a"]] == a) * (delta == 0)) * (pred - est)

    IC <- IC +
      t(colMeans(nabla[data[["a"]] == a & delta == 0,]) %*%
        t(IC(epsilon)))

    estimate(coef = est,
             IC = IC,
             id = id,
             labels = paste0("E[U|A=",a,",delta=0]"))
  }

  est <- lapply(
    c(1,0),
    FUN = fun
  )
  est <- do.call("merge", est)

  return(est)
}
