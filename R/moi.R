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
#' estimates of \eqn{E[U(X,A,Z;\theta)|A=a, \Delta=0]}, for an imputation model
#' \eqn{U}, where \eqn{X} denotes baseline covariates, \eqn{A} denotes the
#' treatment, \eqn{Z} denotes post randomization covariates, and \eqn{\Delta}
#' denotes a non-missing indicator. Influence function based standard errors are
#' also provided.
#' @param id A vector with subject IDs
#' @param treatment.model A \code{learner} object for the binary treatment,
#' used to extract the treatment variable and its levels.
#' @param imputation.model A learner object of class 'learner_glm' used to fit
#' the imputation model. The learner must specify the outcome variable and
#' model formula. If the learner was constructed with user-supplied
#' \code{weights}, those weights are multiplied by the
#' \code{imputation.subset} indicator (excluded rows receive zero weight).
#' @param missing.model \code{learner} object
#' specifying the model for the probability of the outcome being
#' observed/non-missing
#' @param imputation.augmentation.model \code{learner} object
#' specifying the model for the imputation augmentation
#' @param extended.output Logical. If \code{TRUE}, the returned list also
#'   includes the augmentation component \code{IC3} of the influence
#'   function (only when \code{imputation.augmentation = TRUE}) and the
#'   imputation-model influence function \code{IC_epsilon}. Default is
#'   \code{FALSE}.
#' @return A list with components:
#'   \item{estimate}{A [`lava::estimate`] object with coefficients
#'     \eqn{E[U|A=1,\Delta=0]} and \eqn{E[U|A=0,\Delta=0]} and the
#'     associated influence functions.}
#'   \item{imputation.model}{The fitted imputation model.}
#'   \item{imputation.subset}{The \code{imputation.subset} expression.}
#'   \item{levels}{Treatment levels (character).}
#'   \item{IC3}{(only if \code{extended.output = TRUE} and
#'     \code{imputation.augmentation = TRUE}) Named list (one entry per
#'     treatment level) giving the per-level augmentation contribution to
#'     the influence function.}
#'   \item{IC_epsilon}{(only if \code{extended.output = TRUE}) Influence
#'     function for the imputation-model parameters.}
#' @keywords internal
#' @inheritParams moi_missing
moi_missing <- function(data,
                        id,
                        delta,
                        treatment.model,
                        imputation.model,
                        imputation.subset = NULL,
                        imputation.augmentation = FALSE,
                        missing.model = NULL,
                        imputation.augmentation.model = NULL,
                        extended.output = FALSE) {
  ## Input checks
  if (!inherits(imputation.model, "learner_glm")) {
    stop("imputation.model must be of inherited class 'learner_glm'")
  }
  ## Clone so the caller's imputation.model is not fitted in place.
  imputation.model <- imputation.model$clone()
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

  ## Evaluate imputation.subset expression
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
  ## Validate imputation.subset result
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

  extract_weights <- function(lr) {
    if (!is.null(weights <- lr$opt("weights"))) {
      rlang::warn(
        paste0(
          "Provide imputation.model 'weights' via 'specials' and not ",
          "'learner.args' argument."
        ),
        call = call("moi")
      )
    } else {
      weights <- lr$design(data, na.action = na.pass)$weights
    }
    weights
  }

  ## Combine user-specified weights with subset weights.
  user_weights <- extract_weights(imputation.model)
  if (is.null(user_weights)) {
    weights <- as.numeric(model_rows)
  } else {
    if (length(user_weights) != nrow(data)) {
      stop(sprintf(
        "imputation.model 'weights' length (%d) does not match data rows (%d)",
        length(user_weights), nrow(data)
      ))
    }
    if (any(is.na(user_weights))) {
      stop("imputation.model 'weights' must not contain NA values")
    }
    if (any(user_weights < 0)) {
      stop("imputation.model 'weights' must be non-negative")
    }
    weights <- user_weights * as.numeric(model_rows)
  }

  ## Fit imputation model
  imputation.model$estimate(data = data,
                            weights = weights,
                            na.action = lava::na.pass0)

  ## Predict from imputation model
  pred <- imputation.model$predict(newdata = data, type = "response")

  if (isTRUE(extended.output)) {
    IC_epsilon <- IC(imputation.model$fit)
  }

  ## Getting the treatment variable and levels:
  A <- treatment.model$response(data)
  levels <- rev(sort(unique(A)))
  treatment_name <- lava::getoutcome(treatment.model$formula)

  if (isTRUE(imputation.augmentation)) {
    if (!is.null(imputation.augmentation.model)) {
      ## Fitting a model for E[U(X,A,Z;\theta)|W, A]
      imputation.augmentation.model <- imputation.augmentation.model$clone()
      imputation.augmentation.model$update("U_")
      if ("U_" %in% colnames(data)) {
        stop("'U_' column not permitted in data")
      }
      data$U_ <- pred
      imputation.augmentation.model$estimate(data)
      data$U_ <- NULL
    }

    ## Fitting missing model
    missing.model <- missing.model$clone()
    missing.model$update("delta")
    if ("delta" %in% colnames(data)) {
      stop("'delta' column not permitted in data")
    }
    data$delta <- delta
    missing.model$estimate(data = data)
    data$delta <- NULL
  }

  # Getting the estimate for E[U(X,A,Z;\theta)|A = a, \Delta = 0]
  fun <- function(a) {
    newdata <- data
    n_observed_in_arm <- sum((A == a) & (delta == 1))
    n_missing_in_arm  <- sum((A == a) & (delta == 0))

    ## Case: no missing outcomes in this arm, i.e., {A = a, \Delta = 0}
    ## is an empty set.
    ## Setting E[U|A=a, Delta=0] = 0: return a
    ## degenerate zero-coef/zero-IC estimate object
    if (n_missing_in_arm == 0) {
      out <- estimate(coef = 0,
                      IC = matrix(0, nrow = length(id), ncol = 1),
                      id = id,
                      labels = paste0("E[u(", a, ")|d=0]"))
      if (isTRUE(extended.output) && isTRUE(imputation.augmentation)) {
        attr(out, "IC3") <- rep(0, length(id))
      }
      return(out)
    }

    ## Case: all missing in this arm.
    ## Warn the user that this arm is identified solely by the
    ## imputation model.
    if (n_observed_in_arm == 0) {
      warning(sprintf(
        paste0("All outcomes are missing in arm '%s'; the per-arm ",
               "potential outcome E[Y|A=%s] is identified only by the ",
               "imputation model."),
        a, a
      ))
    }

    ## Plug-in estimate
    est <- estimate(imputation.model$fit,
                    predict_glm, # nolint: object_usage_linter.
                    data = data,
                    subset = (A == a) & (delta == 0),
                    average = TRUE,
                    id = seq_len(nrow(data)))
    IC <- IC(est)
    est <- coef(est)

    if (isTRUE(imputation.augmentation)) {

      newdata[[treatment_name]] <- a
      if (!is.null(imputation.augmentation.model)) {
        H <- imputation.augmentation.model$predict(newdata = newdata,
                                                   type = "response")
      } else {
        H <- imputation.model$predict(newdata = newdata, type = "response")
      }

      SW <- missing.model$predict(newdata = newdata, type = "response")
      g <- mean(A == a)
      S <- mean((delta == 1)[A == a])

      ## Get the uncentralized plug-in influence function
      IC_noncen <- IC + est

      ## augmented one-step estimate
      aug2 <- (1 - SW) / (1 - S) * (H - est)
      aug <- (g - (A == a)) / g * aug2
      est <- est + mean(aug)

      ## augmented (centralized) influence function
      aug2 <- (1 - SW) / (1 - S) * (H - est)
      aug <- (g - (A == a)) / g * aug2
      IC3 <- aug + ((A == a) - g) / g * mean(aug2)
      IC <- (IC_noncen - est)  + IC3
      ## centralizing the IC
      IC <- IC - mean(IC)
    }
    out <- estimate(coef = est,
                    IC = IC,
                    id = id,
                    labels = paste0("E[u(", a, ")|d=0]"))

    if (isTRUE(extended.output)) {
      if (isTRUE(imputation.augmentation)) {
        attr(out, "IC3") <- IC3
      }
    }
    return(out)
  }

  est <- lapply(
    levels,
    FUN = fun
  )
  if (isTRUE(extended.output)) {
    ## Stash per-level IC components from attributes before merge() drops them.
    level_names <- as.character(levels)
    if (isTRUE(imputation.augmentation)) {
      IC3 <- stats::setNames(lapply(est, attr, "IC3"), level_names)
    }
  }
  est <- do.call("merge", est)

  out <- list(
    estimate = est,
    imputation.model = imputation.model,
    imputation.subset = imputation.subset,
    levels = as.character(levels)
  )
  if (isTRUE(extended.output)) {
    if (isTRUE(imputation.augmentation)) {
      out$IC3 <- IC3
    }
    out$IC_epsilon <- IC_epsilon
  }

  return(out)
}

##' @title Average Treatment Effect Estimation with Missing Outcome Imputation
##'
##' @description
##' Estimates the Average Treatment Effect (ATE) in settings where the outcome
##' may be missing (not observed for all individuals). The treatment effect
##' implied by a parametric imputation model is targeted directly through an
##' efficient one-step estimator constructed from its influence function
##' (Nordland et al., 2026).
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
##' E[U(X, A, Z; \theta) | A=a, \Delta=0],}
##'
##' and \eqn{\Delta} denotes the non-missing indicator, and \eqn{U} denotes the
##' imputation model possibly depending on baseline covariates \eqn{X}, the
##' treatment \eqn{A}, and a post randomization variable \eqn{Z}.
##'
##' Inference in based on the estimated influence functions (IFs)
##' of the associated (covariate adjusted) one-step estimators.
##'
##' When \code{imputation.augmentation = TRUE}, an augmentation term built from
##' the efficient influence function is added, giving an efficient one-step
##' estimator of the treatment effect implied by the imputation model
##' (Nordland et al., 2026). The augmentation uses a working model for the
##' conditional imputation mean \eqn{E[U(X, A, Z; \theta) \mid W, A]} (see
##' \code{imputation.augmentation.model}) together with the missingness model.
##'
##' If no observations are missing in an arm \eqn{a}, the imputation
##' contribution for that arm vanishes
##' (\eqn{P(\Delta = 0 | A = a) = 0}) and
##' \eqn{E[\tilde{Y} | A = a] = E[Y | A = a]}. If no observations are
##' missing in any arm, \code{moi} reduces to a standard
##' \code{\link{cate}} call with \code{cate.model = ~ 1}.
##' @param data A \code{data.frame} containing all variables required by the
##'   models. \code{data.table} and \code{tbl_df} objects are automatically
##'   coerced to \code{data.frame}.
##' @param response.model A \code{formula} or \code{learner} object
##'   specifying the response/outcome and the associated baseline adjusted
##'   model. If a \code{formula} is provided,
##'   it is automatically wrapped in \code{\link{learner_glm}}. Used to
##'   estimate \eqn{E[\Delta Y | A = a]}.
##' @param treatment.model A base R \code{stats} formula specifying the
##'   binary treatment variable. Only an intercept is allowed on the right-hand
##'   side, e.g., \code{A ~ 1}.
##' @param missing.model A \code{formula} or \code{learner} object
##'   specifying the model for the probability of the outcome being
##'   observed/non-missing
##'   (i.e., \eqn{P(\Delta = 1 | A = a)}). If a \code{formula} is provided,
##'   it is wrapped in \code{learner_glm(..., family = binomial())}. Used to
##'   estimate \eqn{P(\Delta = 0 | A = a)}.
##' @param imputation.model A \code{formula} or \code{learner_glm} object
##'   specifying the missing outcome imputation model. If a \code{formula}
##'   is provided, it is wrapped in \code{\link{learner_glm}}. Used to estimate
##'   \eqn{E[U(X, A, Z; \theta) | A = a, \Delta = 0]}.
##' @param imputation.subset Optional character string giving an R expression
##'   that evaluates to a logical vector indicating which rows of \code{data}
##'   to use when fitting the imputation model. The expression is parsed and
##'   evaluated in the context of \code{data}; for example,
##'   \code{imputation.subset = "!is.na(y)"} restricts the fit to the observed
##'   outcomes. If \code{NULL} (default), all rows are used.
##' @param imputation.augmentation Logical. If \code{TRUE}, an augmentation
##'   term is added to the imputation estimator for improved efficiency.
##'   Default is \code{FALSE}.
##' @param imputation.augmentation.model A \code{formula}, \code{learner},
##'   or \code{NULL} specifying a working model for the conditional imputation
##'   mean \eqn{E[U(X, A, Z; \theta) \mid W, A]}, used to augment the
##'   imputation estimator. Only used when
##'   \code{imputation.augmentation = TRUE}; if \code{NULL}, the imputation
##'   model \eqn{U} itself is used. Default is \code{NULL}.
##' @param return.all Logical. If \code{TRUE}, the returned object includes all
##'   intermediate estimates
##'   in addition to the final ATE estimate. Default is \code{FALSE}.
##' @return An object of class \code{moi.targeted} (inheriting from
##'   \code{targeted}), a list with components:
##'   \describe{
##'     \item{call}{The matched call.}
##'     \item{estimate}{A [`lava::estimate`] object containing
##'       the per-arm
##'       expected potential outcomes \eqn{E[\tilde{Y}|A=a]} and the ATE
##'       contrast \eqn{E[\tilde{Y}|A=1] - E[\tilde{Y}|A=0]}, with
##'       influence-function-based standard errors. Row labels follow the
##'       \code{\link{cate}} convention: per-arm rows are labeled
##'       \if{latex}{\eqn{E[\tilde{y}(1)]}}{E[ỹ(1)]} and
##'       \if{latex}{\eqn{E[\tilde{y}(0)]}}{E[ỹ(0)]}
##'       (or \code{E[tildeY(1)]} / \code{E[tildeY(0)]} in non-UTF-8 locales),
##'       and the contrast row is labeled
##'       \if{latex}{\eqn{E[\tilde{y}(1)]-E[\tilde{y}(0)]}}{E[ỹ(1)]-E[ỹ(0)]}.}
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
##' @references
##'   Nordland, A., Holst, K. K., Redek, D., Pipper, C. B. & Iversen, A. T.
##'   (2026) One-step Outcome Imputation: An Alternative to Multiple
##'   Imputation. arXiv: https://arxiv.org/abs/2606.07174.
##'
##' @seealso
##'   \code{\link{cate}} for Conditional Average Treatment Effect estimation,
##'   \code{\link{learner}} for creating learner objects,
##'   [`lava::estimate`] for combining and transforming estimators
##'
##' @examples
##' sim_moi <- function(n = 1000, ...) {
##'   w <- rnorm(n)
##'   a <- rbinom(n, 1, 0.5)
##'   y <- 1 + a + w + rnorm(n)
##'   ## outcome observed (delta = 1) with probability depending on w
##'   delta <- rbinom(n, 1, lava::expit(1 + w))
##'   y[delta == 0] <- NA
##'   data.frame(y, a, w)
##' }
##'
##' d <- sim_moi(1000)
##' ## ATE with missing outcomes imputed by a working glm model
##' moi(data = d,
##'     response.model = y ~ a + w,
##'     treatment.model = a ~ 1,
##'     missing.model = ~ a + w,
##'     imputation.model = y ~ a + w,
##'     imputation.subset = "!is.na(y)")
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
  if (inherits(missing.model, "formula")) {
    missing.model <- learner_glm(missing.model, family = binomial())
  }
  if (inherits(imputation.model, "formula")) {
    imputation.model <- learner_glm(imputation.model)
  }
  if (inherits(imputation.augmentation.model, "formula")) {
    imputation.augmentation.model <- learner_glm(imputation.augmentation.model)
  }
  if (!identical(class(treatment.model), "formula")) {
    stop(
      "'treatment.model' must be a base R stats formula (e.g., 'a ~ 1'); ",
      "subclassed formulas and learner objects are not supported."
    )
  }
  trm <- terms(treatment.model)
  if (length(attr(trm, "factors")) != 0) {
    stop("only an intercept is allowed in the treatment.model formula")
  }
  if (!is.null(attr(trm, "offset"))) {
    stop("offset terms are not allowed in 'treatment.model'.")
  }
  rm(trm)
  treatment.model <- learner_glm(treatment.model, family = binomial())

  ## Clone models that are updated:
  response.model <- response.model$clone()
  missing.model <- missing.model$clone()

  ## Extract the non-missing indicator \Delta
  response <- response.model$response(data, na.action = stats::na.pass)
  if (is.null(response)) {
    stop("invalid outcome in response.model")
  }
  delta <- !is.na(response)

  ## Short-circuit: if no observations are missing anywhere, moi reduces
  ## to a standard cate() call. Skip the missing-model and imputation
  ## machinery (which would otherwise produce convergence warnings on
  ## degenerate inputs) and return a moi.targeted-shaped result.
  if (all(delta)) {
    outcome_est <- cate(
      cate.model = ~ 1,
      response.model = response.model,
      treatment.model = treatment.model,
      data = data,
      nfolds = nfolds,
      silent = silent,
      stratify = stratify,
      mc.cores = mc.cores,
      second.order = second.order
    )
    ty <- .moi_tilde_y()
    level_labels <- paste0("E[", ty, "(", outcome_est$levels, ")]")
    ate_label <- paste0("[", level_labels[1], "] - [", level_labels[2], "]")
    per_level <- estimate(outcome_est,
                          keep = c(1, 2),
                          id = id,
                          labels = level_labels)
    ate <- estimate(per_level,
                    f = cbind(1, -1),
                    labels = ate_label)
    out <- list(
      call = cl,
      estimate = c(per_level, ate),
      levels = outcome_est$levels
    )
    class(out) <- c("moi.targeted", "targeted")
    return(out)
  }

  # Fit model for E[\Delta Y | A = a]
  response.model$update("delta_response")
  if ("delta_response" %in% colnames(data)) {
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

  ## Reuse the same cross-fitting folds for subsequent cate() calls
  shared_folds <- outcome_est$folds

  # Get the influence function/curve
  outcome_est <- estimate(outcome_est,
                          keep = c(1, 2),
                          id = id,
                          labels = paste0("E[dy(", outcome_levels, ")]"))

  # Fit model for P(Delta = 1 | A = a)
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

  # Calculate P(Delta = 0 | A = a) and get the influence curve/function
  missing_est <- estimate(missing_est, keep = c(1, 2), id = id)
  missing_est <- estimate(missing_est,
                          f = function(x) 1 - x,
                          labels = paste0(
                            "P(1-d(", missing_levels, "))"
                          ))

  # Fit model for E[U(X,A,Z; theta)|A = a, Delta = 0]
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

  ##  Merging estimates
  est <- c(outcome_est, missing_est, moi_missing_est)
  ## Per-level expected potential outcome under imputation:
  ## E[\tilde{Y}|A=a] = E[\Delta Y|A=a] + P(\Delta=0|A=a) * E[U|A=a, \Delta=0]
  ty <- .moi_tilde_y()
  level_labels <- paste0("E[", ty, "(", missing_levels, ")]")
  per_level <- est[1:2] + est[3:4] * est[5:6]
  per_level <- estimate(per_level, labels = level_labels)

  ## ATE contrast row: [E[ty(1)] - [E[ty(0)]]
  ate_label <- paste0("[", level_labels[1], "] - [", level_labels[2], "]")
  ate <- estimate(per_level, f = cbind(1, -1), labels = ate_label)

  res <- list(
    call = cl,
    estimate = c(per_level, ate),
    levels = missing_levels
  )
  if (isTRUE(return.all)) {
    res$estimate <- merge(per_level, ate, est)
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
