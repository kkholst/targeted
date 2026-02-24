#' Mean Missing Outcome Imputation (MOI)
#'
#' Estimates the mean of a given parametric imputation model among observations
#' with a missing outcome and a given treatment. Specifically, it provides
#' estimates of \eqn{E[U(X,A,Z,\theta)|A=a, \Delta=0]}, for an imputation model
#' \eqn{U}, where \eqn{X} denotes baseline covariates, \eqn{A} denotes the
#' treatment, \eqn{Z} denotes post randomization covariates, and \eqn{\Delta}
#' denotes a non-missing indicator. Influence function based standard errors are
#' also provided.
#' @param data A data.frame containing the analysis dataset. Must include
#'   columns 'id' (unique identifier) and 'a' (binary treatment variable with
#'   values 0 or 1). Data.table and tibble objects will be coerced to
#'   data.frame.
#' @param delta A vector with the non-missing indicator
#' @param treatment.model
#' @param levels A vector of the unique treatment levels
#' @param learner A learner object of class 'learner_glm' used to fit the
#'   imputation model. The learner must specify the outcome variable and model
#'   formula.
#' @param subset Optional. A character string containing an R expression that
#'   evaluates to a logical vector indicating which rows to use for fitting the
#'   imputation model. The expression is evaluated in the context of 'data'. If
#'   NULL (default), all rows are used.
#'
#' @return An estimate object containing:
#'   \item{coef}{Estimates for \eqn{E[U|A=1,\Delta=0]} and
#'    \eqn{E[U|A=0,\Delta=0]}}
#'   \item{IC}{Influence curve values for each observation}
#'   \item{id}{Observation identifiers}
moi <- function(data,
                delta,
                treatment.model,
                learner,
                subset = NULL) {
  ## input checks
  if (!inherits(learner, "learner_glm")) {
    stop("imputation model/learner must be of inherited class 'learner_glm'")
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
  required_cols <- c("id")
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

  ## evaluate subset expression
  if (!is.null(subset)) {
    tryCatch({
      model_rows <- eval(parse(text = subset),
                         envir = data, enclos = parent.frame())
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
    stop(sprintf(
      "'subset' expression length (%d) does not match data rows (%d)",
              length(model_rows), nrow(data)))
  }
  if (any(is.na(model_rows))) {
    stop("'subset' expression cannot produce NA values")
  }
  if (!any(model_rows)) {
    stop("'subset' expression excludes all rows (no TRUE values)")
  }

  # validate rows are available
  if (!any(model_rows)) {
    stop("No observations with non-missing outcome in the selected subset. Cannot fit imputation model.") # nolint
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
  family <- learner$fit$family
  if (inherits(family, "family")) {
    family <- family$family
  }
  link <- learner$fit$family$link
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


  # getting the estimate for E[U(X,A;\theta)|A = a, \Delta = 0]
  fun <- function(a) {
    est <- mean(pred[delta == 0 & A == a])

    IC <- (A == a) * (delta == 0) /
      mean((A == a) * (delta == 0)) * (pred - est)

    IC <- IC +
      t(colMeans(nabla[A == a & delta == 0, ]) %*%
        t(IC(epsilon)))

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
    learner = learner,
    subset = subset,
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
                   transform = NULL,
                   back.transform = NULL,
                   return.all = FALSE) {
  ## TODO: check that the missing reponse and treatment strata are well defined
  n <- nrow(data)
  if (inherits(data, c("data.table", "tbl_df"))) {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (n == 0) {
    stop("'data' cannot be empty (0 rows)")
  }
  required_cols <- c("id")
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

  ## extract the non-missing indicator
  ## updating the outcome response model
  ## updating the non-missing model
  response <- response.model$response(data, na.action = stats::na.pass)
  delta <- !is.na(response)

  if ("delta" %in% colnames(data)) {
    stop("'delta' column not permitted in data")
  }
  data$delta <- delta
  if ("delta_reponse" %in% colnames(data)) {
    stop("'delta_response' column not permitted in data")
  }
  data$delta_response <- ifelse(!delta, 0, response)
  response.model$update("delta_response")
  missing.model$update("delta")

  # fit model for E[\Delta Y | A = a]
  outcome_model <- cate(
    cate.model =  ~ 1,
    response.model = response.model,
    propensity.model = treatment.model,
    data = data
  )

  # get the influence function/curve
  outcome_est <- estimate(outcome_model,
                          keep = c(1, 2),
                          id = data$id,
                          labels = paste0("E[DY|A=", outcome_model$levels, "]"))



  # fit model for P(Delta = 1 | A = a)
  missing_model <- cate(
    cate.model = ~ 1,
    response.model = missing.model,
    propensity.model = treatment.model,
    data = data
  )
  # calculate P(Delta = 0 | A = a) and get the influence curve/function
  missing_est <- estimate(missing_model, keep = c(1, 2), id = data$id)
  missing_est <- estimate(missing_est,
                          f = function(x) 1 - x,
                          labels = paste0(
                            "P(D=0|A=", missing_model$levels, ")"
                          ))

  # fit model for E[U(X,A,Z; theta)|A = a, Delta = 0]
  moi_model <- moi(data = data,
                   delta = delta,
                   treatment.model = treatment.model,
                   learner = imputation.model,
                   subset = imputation.subset)
  moi_est <- moi_model$estimate
  moi_levels <- moi_model$levels

  if (!(identical(missing_model$levels, outcome_model$levels) & identical(missing_model$levels, moi_model$levels))) {
    stop("treatment levels are not identical")
  }

  ##  output
  est <- merge(outcome_est, missing_est, moi_est)
  ate <- estimate(est,
                  f = function(x) x[1:2] + x[3:4] * x[5:6],
                  labels = paste0("E[tildeY|A=", missing_model$levels, "]"))
  ate <- estimate(ate, f = cbind(1, -1), labels = "ATE")
  ## transform and back transform
  ate <- estimate(ate, f = transform, back.transform = back.transform)

  if (return.all == TRUE) {
    ate <- merge(est, ate)
  }

  return(ate)
}
