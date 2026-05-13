## Label helper: Unicode "y with tilde" (U+1EF9) when locale is UTF-8;
## ASCII fallback "tildeY" otherwise. Used to render E[\tilde{y}(a)] labels
## in moi() output.
.moi_tilde_y <- function() {
  if (isTRUE(l10n_info()[["UTF-8"]])) "\u1ef9" else "tildeY"
}

#' Mean Imputation Among Missing Outcomes
#'
#' Estimates the mean of a given parametric imputation model among observations
#' with a missing outcome and a given treatment. Specifically, it provides
#' estimates of \eqn{E[U(X,A,Z,\theta)|A=a, \Delta=0]}, for an imputation model
#' \eqn{U}, where \eqn{X} denotes baseline covariates, \eqn{A} denotes the
#' treatment, \eqn{Z} denotes post randomization covariates, and \eqn{\Delta}
#' denotes a non-missing indicator. Influence function based standard errors are
#' also provided.
#' @param data A data.frame containing the analysis dataset. Data.table and
#' tibble objects will be coerced to
#'   data.frame.
#' @param delta A vector with the non-missing indicator
#' @param treatment.model Learner object
#' @param imputation.model A learner object of class 'learner_glm' used to fit
#' the imputation model. The learner must specify the outcome variable and
#' model formula.
#' @param imputation.subset Optional. A character string containing an R
#' expression that evaluates to a logical vector indicating which rows to use
#' for fitting the
#' imputation model. The expression is evaluated in the context of 'data'. If
#' NULL (default), all rows are used.
#' @param imputation.augmentation Logical. Should an augmentation term
#' associated with the imputation model be added to the one-step estimator
#' @param missing.model \code{learner} object
##'   specifying the model for the probability of the outcome being
##'   observed/non-missing
#' @param imputation.augmentation.model \code{learner} object
##'   specifying the model for the imputation augmentation
#' @return An estimate object containing:
#'   \item{coef}{Estimates for \eqn{E[U|A=1,\Delta=0]} and
#'    \eqn{E[U|A=0,\Delta=0]}}
#'   \item{IC}{Influence curve values for each observation}
#'   \item{id}{Observation identifiers}
moi_missing <- function(data,
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
      stop(sprintf("Error evaluating 'imputation.subset' expression: %s",
                   e$message))
    })
  } else {
    model_rows <- rep(TRUE, times = nrow(data))
  }
  # validate imputation.subset result
  if (!is.logical(model_rows)) {
    stop("'imputation.subset' expression must evaluate to a logical vector")
  }
  if (length(model_rows) != nrow(data)) {
    stop(
      sprintf(
        "'imputation.subset' expression length (%d) does not match data rows (%d)", # nolint
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
  treatment_name <- lava::getoutcome(treatment.model$formula)


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
  fun <- function(a) {
    newdata <- data
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
      (g * (1 - S)) * (pred - est)

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
             labels = paste0("E[u(", a, ")|d=0]"))
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

##' @title Average Treatment Effect Estimation with Missing Outcome Imputation
##'
##' @description
##' Estimates the Average Treatment Effect (ATE) in settings where the outcome
##' may be missing (not observed for all individuals). The function uses an
##' imputation-based approach combined with doubly robust estimation techniques.
##'
##' @details
##' The \code{moi} function implements an estimator for the Average Treatment
##' Effect where missing outcomes are imputed using a parametric (glm) model.
##'
##' The function estimate the target parameter
##'
##' \deqn{E[\tilde{Y}| A = 1] - E[\tilde{Y}| A = 0],}
##'
##' where
##'
##' \deqn{E[\tilde{Y}| A = a] = E[\Delta Y | A=a] + P(\Delta=0 | A=a) \cdot
##' E[U(X, A, Z, \theta) | A=a, \Delta=0],}
##'
##' and \eqn{\Delta} denotes the non-missing indicator, and \eqn{U} denotes the
##' imputation model possibly depending on baseline covariates \eqn{X}, the
##' treatment \eqn{A}, and a post randomization variable \eqn{Z}.
##'
##' Inference in based on the estimated influence functions (IFs)
##' of the associated (covariate adjusted) one-step estimators.
##'
##' @param data A \code{data.frame} containing all variables required by the
##'   models. \code{data.table} and \code{tbl_df} objects are automatically
##'   coerced to \code{data.frame}.
##'
##' @param response.model A \code{formula} or \code{learner} object
##'   specifying the response/outcome and the associated baseline adjusted
##'   model. If a \code{formula} is provided,
##'   it is automatically wrapped in \code{\link{learner_glm}}. Used to
##'   estimate \eqn{E[\Delta Y | A = a]}.
##'
##' @param treatment.model A \code{formula}
##'   specifying the binary treatment variable, e.g., A ~ 1.
##'
##' @param missing.model A \code{formula} or \code{learner} object
##'   specifying the model for the probability of the outcome being
##'   observed/non-missing
##'   (i.e., \eqn{P(\Delta = 1 | A = a)}). If a \code{formula} is provided,
##'   it is wrapped in \code{learner_glm(..., family = binomial())}. Used to
##'   estimate \eqn{P(\Delta = 0 | A = a)}.
##'
##' @param imputation.model A \code{formula} or \code{learner_glm} object
##'   specifying the missing outcome imputation model. If a \code{formula}
##'   is provided, it is wrapped in \code{\link{learner_glm}}. Used to estimate
##'   \eqn{E[U(X, A, Z; \theta) | A = a, \Delta = 0]}.
##'
##' @param imputation.subset An optional logical vector specifying a subset of
##'   the data to use when fitting the imputation model. Default is \code{NULL}
##'   (all observations are used).
##'
##' @param imputation.augmentation Logical. If \code{TRUE}, an augmentation
##'   term is added to the imputation estimator for improved efficiency.
##'   Default is \code{FALSE}.
##'
##' @param imputation.augmentation.model A \code{formula}, \code{learner},
##'   or \code{NULL} specifying the model used for the augmentation of the
##'   imputation estimator. Only used if \code{imputation.augmentation = TRUE}.
##'   Default is \code{NULL}.
##'
##' @param return.all Logical. If \code{TRUE}, the returned object includes all
##'   intermediate estimates
##'   in addition to the final ATE estimate. Default is \code{FALSE}.
##'
##' @return An object of class \code{moi.targeted} (inheriting from
##'   \code{targeted}), a list with components:
##'   \describe{
##'     \item{call}{The matched call.}
##'     \item{estimate}{A \code{lava::estimate} object containing the per-arm
##'       expected potential outcomes \eqn{E[\tilde{Y}|A=a]} and the ATE
##'       contrast \eqn{E[\tilde{Y}|A=1] - E[\tilde{Y}|A=0]}, with
##'       influence-function-based standard errors. Row labels follow the
##'       \code{\link{cate}} convention: per-arm rows are labeled
##'       \code{E[ỹ(1)]} and \code{E[ỹ(0)]} (or
##'       \code{E[tildeY(1)]} / \code{E[tildeY(0)]} in non-UTF-8 locales),
##'       and the contrast row is labeled
##'       \code{[E[ỹ(1)]] - [E[ỹ(0)]]}.}
##'     \item{levels}{Treatment levels (character).}
##'     \item{intermediate}{(only if \code{return.all = TRUE}) Intermediate
##'       estimates: \eqn{E[\Delta Y|A=a]}, \eqn{P(\Delta=0|A=a)}, and
##'       \eqn{E[U|A=a, \Delta=0]}.}
##'   }
##'   Standard methods (\code{print}, \code{summary}, \code{coef},
##'   \code{vcov}, \code{IC}) are provided.
##'
##' @inheritParams cate
##'
##' @author Andreas Nordland
##'
##' @seealso
##'   \code{\link{cate}} for Conditional Average Treatment Effect estimation,
##'   \code{\link{learner}} for creating learner objects,
##'   \code{\link{lava::estimate}} for combining and transforming estimators
##'
##' @export
moi <- function(data,
                response.model,
                treatment.model,
                missing.model,
                imputation.model,
                imputation.subset = NULL,
                imputation.augmentation = FALSE,
                imputation.augmentation.model = NULL,
                return.all = FALSE,
                nfolds = 1,
                silent = FALSE,
                stratify = FALSE,
                mc.cores = NULL,
                second.order = TRUE) {
  ## TODO: check that the missing reponse and treatment strata are well defined
  ## TODO: bug when parameters are NA in the imputation model
  cl <- match.call()
  n <- nrow(data)
  id <- seq_len(nrow(data))
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

  ## check that treatment.model is a learner_glm with family = "binomial",
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
    treatment.model = treatment.model,
    data = data,
    nfolds = nfolds,
    silent = silent,
    stratify = stratify,
    mc.cores = mc.cores,
    second.order = second.order
  )
  data$delta_response <- NULL
  outcome_levels <- outcome_est$levels
  ## reuse the same cross-fitting folds for subsequent cate() calls
  shared_folds <- outcome_est$folds

  # get the influence function/curve
  outcome_est <- estimate(outcome_est,
                          keep = c(1, 2),
                          id = id,
                          labels = paste0("E[dy(", outcome_levels, ")]"))



  # fit model for P(Delta = 1 | A = a)
  missing.model$update("delta")
  if ("delta" %in% colnames(data)) {
    stop("'delta' column not permitted in data")
  }
  data$delta <- delta
  missing_est <- cate(
    cate.model = ~ 1,
    response.model = missing.model,
    treatment.model = treatment.model,
    data = data,
    nfolds = shared_folds,
    silent = silent,
    stratify = stratify,
    mc.cores = mc.cores,
    second.order = second.order
  )
  data$delta <- NULL
  missing_levels <- missing_est$levels

  # calculate P(Delta = 0 | A = a) and get the influence curve/function
  missing_est <- estimate(missing_est, keep = c(1, 2), id = id)
  missing_est <- estimate(missing_est,
                          f = function(x) 1 - x,
                          labels = paste0(
                            "P(1-d(", missing_levels, "))"
                          ))

  # fit model for E[U(X,A,Z; theta)|A = a, Delta = 0]
  moi_missing_est <- moi_missing(
    data = data,
    id = id,
    delta = delta,
    treatment.model = treatment.model,
    imputation.model = imputation.model,
    imputation.subset = imputation.subset,
    imputation.augmentation = imputation.augmentation,
    imputation.augmentation.model = imputation.augmentation.model,
    missing.model = missing.model
  )
  moi_missing_levels <- moi_missing_est$levels
  moi_missing_est <- moi_missing_est$estimate

  if (!(identical(missing_levels, outcome_levels) &&
        identical(missing_levels, moi_missing_levels))) {
    stop("treatment levels are not identical")
  }

  ##  output
  est <- merge(outcome_est, missing_est, moi_missing_est)
  ## per-arm expected potential outcome under imputation:
  ## E[\tilde{Y}|A=a] = E[\Delta Y|A=a] + P(\Delta=0|A=a) * E[U|A=a, \Delta=0]
  ty <- .moi_tilde_y()
  level_labels <- paste0("E[", ty, "(", missing_levels, ")]")
  etilde <- est[1:2] + est[3:4] * est[5:6]
  etilde <- estimate(etilde, labels = level_labels)

  ## ATE contrast row: [E[ty(1)] - [E[ty(0)]]
  ate_label <- paste0("[", level_labels[1], "] - [", level_labels[2], "]")
  ate_row <- estimate(etilde, f = cbind(1, -1), labels = ate_label)

  ## per-level rows + contrast row, mirroring cate.targeted$estimate layout
  estimate_full <- merge(etilde, ate_row)

  res <- list(
    call = cl,
    estimate = estimate_full,
    levels = missing_levels
  )
  if (isTRUE(return.all)) {
    res$intermediate <- est
    res$estimate <- merge(estimate_full, est)
  }
  class(res) <- c("moi.targeted", "targeted")
  return(res)
}

#' @export
summary.moi.targeted <- function(object, ...) {
  B <- rbind(rep(0, length(coef(object))))
  B[1:2] <- c(1, -1)
  obj <- structure(list(
    estimate = object$estimate,
    call = object$call,
    ate = lava::estimate(object$estimate, B)
  ), class = "summary.moi.targeted")
  return(obj)
}

#' @export
print.summary.moi.targeted <- function(x, ...) {
  print(x$call)
  cat("\n")
  print(x$estimate, ...)
  cat("\nAverage Treatment Effect:\n")
  print(x$ate)
}
