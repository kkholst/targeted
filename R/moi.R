## Label helper: Unicode "y with tilde" (U+1EF9) when locale is UTF-8;
## ASCII fallback "tildeY" otherwise. Used to render E[\tilde{y}(a)] labels
## in moi() output.
.moi_tilde_y <- function() {
        if (isTRUE(l10n_info()[["UTF-8"]])) "\u1ef9" else "tildeY"
}

## Fit the imputation model and build the model-agnostic context used by both
## the sub-target estimator (moi_missing) and the direct one-step estimator
## (moi_direct).
##
## Responsible for: validating the imputation.model, cloning it (so the
## caller's object is never fitted in place), evaluating/validating the
## imputation.subset expression, merging any user-supplied weights with the
## subset indicator, fitting the model, and exposing the seam needed to call
## lava::expectation() on the imputation parameter:
##   - model : the fitted (cloned) learner
##   - object: estimate(fit, id = id), the influence-function carrier
##   - f     : the prediction function passed to lava::expectation()
##   - data  : the data.frame predictions are evaluated on
##   - pred  : point predictions U(.;theta.hat) on every row of `data`
##
## Currently supports learner_glm imputation models; the mmrm branch will slot
## in here without changing the downstream estimators.
.moi_imputation_context <- function(data, id, imputation.model,
                                    imputation.subset = NULL) {
  ## Input checks
  if (!inherits(imputation.model, "learner_glm")) {
    stop("imputation.model must be of inherited class 'learner_glm'")
  }
  ## Clone so the caller's imputation.model is not fitted in place.
  imputation.model <- imputation.model$clone()

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

  list(
    model = imputation.model,
    object = estimate(imputation.model$fit, id = id),
    f = predict,
    data = data,
    pred = imputation.model$predict(newdata = data, type = "response")
  )
}

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

  ## Fit the imputation model
  ## Use positional ids (1..n) for the imputation object to keep the order of
  ## the influence function stable regardless of the caller's `id`.
  imputation <- .moi_imputation_context(
    data = data,
    id = seq_len(nrow(data)),
    imputation.model = imputation.model,
    imputation.subset = imputation.subset
  )
  imputation.model <- imputation$model
  pred <- imputation$pred

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
        paste0("All outcomes are missing in arm '%s': ",
               "E[tilde Y|A=%s] is identified only by the ",
               "imputation model."),
        a, a
      ))
    }

    ## Plug-in estimate
    subset_ <- (A == a) & (delta == 0)
    est <- lava::expectation(
                    f = imputation$f,
                    object = imputation$object,
                    data = imputation$data,
                    subset = subset_,
                    id = seq_len(nrow(data)) # keep order of the IC
                  )
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

## Direct one-step imputation estimator of E[tilde Y(theta) | A = a].
##
## Implements the estimator of supplementary material C of Nordland et al.
## (2026): a one-step estimator applied to the imputed outcome
##   tilde Y = Delta * Y + (1 - Delta) * U(X, A, Z; theta.hat),
## with an additional influence-function term correcting for estimation of the
## imputation parameter theta.
##
## The influence function (per arm a) is
##   phi(a) = I(A=a)/g(a) {tilde Y - Q*(X,A)} + Q*(X,a) - Psi(a)   [term 1]
##          + [g(a) - I(A=a)] / g(a) E[I(A=a)/g(a) {tilde Y - Q*(X,A)}] [term 2]
##          + E[ I(A=a)/g(a) (1-Delta) nabla U(theta) ] epsilon(.)   [term 3]
## Terms 1-2 are exactly what cate(cate.model = ~ 1) returns for the imputed
## outcome. Term 3 is obtained from lava::expectation(..., details = TRUE):
## attr(., "ic.theta") equals E[S nabla U(theta)] %*% epsilon with
## S = {A = a, Delta = 0}, so dividing by g(a) gives term 3.
##
## `imputation` is the context returned by .moi_imputation_context().
## Returns a list with `per_level` (lava::estimate with the two per-arm
## potential outcomes and their influence functions), `levels`,
## `imputation.model`, and `intermediate` (the uncorrected cate() per-arm
## estimate, i.e. terms 1-2 only).
moi_direct <- function(data,
                       id,
                       delta,
                       response,
                       response.model,
                       treatment.model,
                       imputation,
                       nfolds = 1,
                       silent = FALSE,
                       stratify = FALSE,
                       mc.cores = NULL,
                       second.order = TRUE) {
  ## Imputed outcome:
  if ("imputed_response" %in% colnames(data)) {
    stop("'imputed_response' column not permitted in data")
  }
  imputed_response <- response
  imputed_response[!delta] <- imputation$pred[!delta]
  if (anyNA(imputed_response)) {
    stop(sprintf(
      paste0("the imputation model produced NA predictions for %d ",
             "observation(s) with a missing outcome; check for missing ",
             "covariates in 'imputation.model'."),
      sum(is.na(imputed_response))))
  }
  data$imputed_response <- imputed_response
  response.model$update("imputed_response")

  ## One-step imputation estimator
  cate_est <- cate(
    cate.model = ~ 1,
    response.model = response.model,
    treatment.model = treatment.model,
    data = data,
    id = id,
    nfolds = nfolds,
    silent = silent,
    stratify = stratify,
    mc.cores = mc.cores,
    second.order = second.order
  )
  data$imputed_response <- NULL

  est <- estimate(cate_est, keep = c(1, 2))

  ## Influence function correction for the imputation parameter.
  A <- treatment.model$response(data)
  levels <- rev(sort(unique(A)))
  if (!identical(as.character(levels), cate_est$levels)) {
    stop("treatment levels are not identical")
  }

  fun <- function(a) {
    label <- paste0("ic_theta(", a, ")")
    in_arm <- (A == a)
    ## Empty subset {A = a, Delta = 0}: return a degenerate
    ## zero-coef/zero-IC estimate.
    if (sum(in_arm & !delta) == 0) {
      return(estimate(coef = 0,
                      IC = matrix(0, nrow = length(id), ncol = 1),
                      id = id,
                      labels = label))
    }
    ## All missing in this arm: identified solely by the imputation model.
    if (sum(in_arm & delta) == 0) {
      warning(sprintf(
        paste0("All outcomes are missing in arm '%s': ",
               "E[tilde Y|A=%s] is identified only by the ",
               "imputation model."),
        a, a
      ))
    }
    ex <- lava::expectation(
      f = imputation$f,
      object = imputation$object,
      data = imputation$data,
      subset = in_arm & (!delta),
      id = id,
      details = TRUE
    )
    ## attr(., "ic.theta") = E[S nabla U(theta)] %*% epsilon
    ic_theta <- attr(ex, "ic.theta") / mean(in_arm)
    estimate(coef = 0,
             IC = ic_theta[, 1],
             id = rownames(ic_theta),
             labels = label)
  }
  correction <- do.call("merge", lapply(levels, fun))

  ## per-level estimate object
  per_level <- est + correction

  list(
    per_level = per_level,
    levels = as.character(levels),
    imputation.model = imputation$model,
    intermediate = est
  )
}

## Sub-target (indirect) estimator of E[tilde Y(theta) | A = a], decomposing
## the target into E[Delta Y | A = a], P(Delta = 0 | A = a) and
## E[U(X,A,Z;theta) | A = a, Delta = 0] (see Nordland et al. 2026, Estimation).
## Requires a missingness model. An augmentation term is added to the
## imputation sub-target whenever `imputation.augmentation.model` is supplied.
## Returns a list with `per_level` (unlabeled lava::estimate of the two per-arm
## potential outcomes), `levels`, and `intermediate` (the merged sub-target
## estimates, for return.all).
moi_indirect <- function(data,
                         id,
                         delta,
                         response,
                         response.model,
                         treatment.model,
                         missing.model,
                         imputation.model,
                         imputation.subset = NULL,
                         imputation.augmentation.model = NULL,
                         nfolds = 1,
                         silent = FALSE,
                         stratify = FALSE,
                         mc.cores = NULL,
                         second.order = TRUE) {
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
    id = id,
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
                          labels = paste0("E[dy(", outcome_levels, ")]"))

  ## Fit model for P(Delta = 1 | A = a)
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
    id = id,
    nfolds = shared_folds,
    silent = silent,
    stratify = stratify,
    mc.cores = mc.cores,
    second.order = second.order
  )
  data$delta <- NULL
  missing_levels <- missing_est$levels

  ## Calculate P(Delta = 0 | A = a) and get the influence curve/function
  missing_est <- estimate(missing_est, keep = c(1, 2))
  missing_est <- estimate(missing_est,
                          f = function(x) 1 - x,
                          labels = paste0(
                            "P(1-d(", missing_levels, "))"
                          ))

  ## Fit model for E[U(X,A,Z; theta)|A = a, Delta = 0]
  moi_missing_est <- moi_missing(
    data = data,
    id = id,
    delta = delta,
    treatment.model = treatment.model,
    imputation.model = imputation.model,
    imputation.subset = imputation.subset,
    imputation.augmentation = !is.null(imputation.augmentation.model),
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
  per_level <- est[1:2] + est[3:4] * est[5:6]

  list(
    per_level = per_level,
    levels = missing_levels,
    intermediate = est
  )
}

#' @title average treatment effect estimation with Missing Outcome Imputation
#'
#' @description
#' Estimates the average treatment effect in settings where the outcome
#' may be missing. The treatment effect
#' implied by a parametric imputation model is targeted directly through an
#' efficient one-step estimator
#' (Nordland et al., 2026).
#'
#' @details
#' The \code{moi} function implements an estimator for the average treatment
#' effect where missing outcomes are imputed using a parametric (glm) model.
#'
#' The function estimate the target parameter
#'
#' \deqn{E[\tilde{Y}| A = 1] - E[\tilde{Y}| A = 0],}
#'
#' where
#'
#' \deqn{E[\tilde{Y}| A = a] = E[\Delta Y | A=a] + P(\Delta=0 | A=a)
#' E[U(X, A, Z; \theta) | A=a, \Delta=0],}
#'
#' and \eqn{\Delta} denotes the non-missing indicator, and \eqn{U} denotes the
#' imputation model possibly depending on baseline covariates \eqn{X}, the
#' treatment \eqn{A}, and a post randomization variable \eqn{Z}.
#'
#' Inference is based on the estimated influence function
#' of the associated (covariate adjusted) one-step estimator.
#'
#' Two estimators are available, selected by the \code{type} argument:
#' \itemize{
#'   \item \strong{Direct one-step imputation estimator}
#'     (\code{type = "direct"}, the default): targets
#'     \eqn{E[\tilde{Y}(\theta) | A = a]} directly via a (covariate augmented)
#'     one-step estimator
#'     applied to the imputed outcome
#'     \eqn{\tilde{Y} = \Delta Y + (1-\Delta) U(X,A,Z;\theta)},
#'     see supplementary material C of Nordland et al. (2026).
#' \item \strong{Sub-target (indirect) estimator} (\code{type = "indirect"}):
#'     estimates the three sub-target parameters
#'     \eqn{E[\Delta Y | A=a]}, \eqn{P(\Delta=0 | A=a)} and
#'     \eqn{E[U(X,A,Z;\theta) | A=a, \Delta=0]} separately and combines them.
#'     A missingness model is required for (augmented) estimation of
#'     \eqn{P(\Delta=0 | A=a)} and for the optional imputation model
#'     augmentation, see supplementary material B of Nordland et al. (2026).
#' }
#'
#' @param response.model formula or learner object (formula => learner_glm).
#'   For the direct estimator, the outcome model for the imputed outcome
#'   \eqn{\tilde{Y}(\theta)}
#' @param treatment.model formula specifying the binary
#'   treatment. Only an intercept is allowed on the right-hand side, e.g.
#'   `a ~ 1`
#' @param imputation.model formula or learner_glm object (formula =>
#'   learner_glm). Model \eqn{U(X,A,Z;\theta)} used to impute the missing
#'   outcomes
#' @param imputation.subset character string with an R expression evaluating to
#'   a logical vector indicating the rows of `data` used to fit the imputation
#'   model, e.g. `"!is.na(y)"`. The expression is evaluated in the context of
#'   `data`. When `NULL` (default), all rows are used
#' @param data data.frame (`data.table` and `tbl_df` objects are coerced to
#'   `data.frame`)
#' @param type character specifying the estimator. `"direct"` (default) gives
#'   the direct one-step imputation estimator, which uses no missingness
#'   model. `"indirect"` gives the sub-target estimator, which requires
#'   `missing.model`
#' @param id (integer or character) optional subject id vector of length
#'   `nrow(data)`.
#' @param missing.model formula or learner object (formula => learner_glm(family
#'   = binomial)); default `NULL`. Model for the probability of the outcome
#'   being missing, \eqn{P(\Delta = 0 | A = a)}. Required when
#'   `type = "indirect"` and ignored when `type = "direct"`
#' @param imputation.augmentation.model formula or learner object (formula =>
#'   learner_glm); default `NULL`. Working model for the conditional imputation
#'   mean \eqn{E[U(X, A, Z; \theta) \mid W, A]}. When supplied, an augmentation
#'   term is added to the imputation sub-target for improved efficiency. Only
#'   used when `type = "indirect"`
#' @param return.all if TRUE the `estimate` component additionally includes the
#'   intermediate estimates: \eqn{E[\Delta Y|A=a]}, \eqn{P(\Delta=0|A=a)} and
#'   \eqn{E[U|A=a, \Delta=0]} for the sub-target estimator, and
#'   \eqn{E[\tilde{Y}| A = a] without the imputation parameter
#'   influence function correction for the direct
#'   estimator
#' @inheritParams cate
#' @return moi.targeted object
#' @author Andreas Nordland
#' @references Nordland, A., Holst, K. K., Redek, D., Pipper, C. B. & Iversen,
#'   A. T. (2026) One-step Outcome Imputation: An Alternative to Multiple
#'   Imputation. arXiv: https://arxiv.org/abs/2606.07174.
#' @examples
#' sim_moi <- function(n = 1000, ...) {
#'   w <- rnorm(n)
#'   a <- rbinom(n, 1, 0.5)
#'   y <- 1 + a + w + rnorm(n)
#'   ## outcome observed (delta = 1) with probability depending on w
#'   delta <- rbinom(n, 1, lava::expit(1 + w))
#'   y[delta == 0] <- NA
#'   data.frame(y, a, w)
#' }
#'
#' d <- sim_moi(1000)
#' ## direct one-step imputation estimator
#' moi(response.model = y ~ a + w,
#'     treatment.model = a ~ 1,
#'     imputation.model = y ~ a + w,
#'     imputation.subset = "!is.na(y)",
#'     data = d)
#'
#' ## sub-target estimator
#' moi(response.model = y ~ a + w,
#'     treatment.model = a ~ 1,
#'     imputation.model = y ~ a + w,
#'     imputation.subset = "!is.na(y)",
#'     missing.model = ~ a + w,
#'     data = d,
#'     type = "indirect")
#'
#' @export
moi <- function(response.model,
                treatment.model,
                imputation.model,
                imputation.subset = NULL,
                data,
                type = c("direct", "indirect"),
                id = NULL,
                imputation.augmentation.model = NULL,
                missing.model = NULL,
                nfolds = 1,
                silent = FALSE,
                stratify = FALSE,
                mc.cores = NULL,
                second.order = TRUE,
                return.all = FALSE) {
  cl <- match.call()
  n <- nrow(data)

  type <- match.arg(type)
  indirect <- identical(type, "indirect")

  if (is.null(id)) {
    id <- seq_len(nrow(data))
  }
  if (!is.null(id)) {
    if (!is.vector(id)) {
      rlang::abort("subject ids must be a vector")
    }
    if (length(id) != n) {
      rlang::abort("subject ids must be a vector of length `nrow(data)`")
    }
  }

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
  if (indirect && is.null(missing.model)) {
    stop(sprintf("'missing.model' is required when type = '%s'.", type))
  }
  if (!is.null(missing.model) && inherits(missing.model, "formula")) {
    missing.model <- learner_glm(missing.model, family = binomial())
  }
  if (inherits(imputation.model, "formula")) {
    imputation.model <- learner_glm(imputation.model)
  }
  if (inherits(imputation.augmentation.model, "formula")) {
    imputation.augmentation.model <- learner_glm(imputation.augmentation.model)
  }
  if (!is.null(imputation.augmentation.model) &&
        !inherits(imputation.augmentation.model, "learner")) {
    stop(
      "'imputation.augmentation.model' must be a formula or a learner ",
      "object."
    )
  }
  if (!indirect) {
    ignored <- c("missing.model", "imputation.augmentation.model")[
      c(!is.null(missing.model), !is.null(imputation.augmentation.model))
    ]
    if (length(ignored) > 0) {
      warning(sprintf(
        paste0("%s ignored when type = 'direct'; the direct one-step ",
               "imputation estimator uses no missingness model. Use ",
               "type = 'indirect' for the sub-target estimator."),
        paste0("'", ignored, "'", collapse = " and ")))
    }
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

  ## Clone updated models
  response.model <- response.model$clone()
  if (!is.null(missing.model)) {
    missing.model <- missing.model$clone()
  }

  ## Extract the non-missing indicator \Delta
  response <- response.model$response(data, na.action = stats::na.pass)
  if (is.null(response)) {
    stop("invalid outcome in response.model")
  }
  delta <- !is.na(response)

  ## No missing outcomes: cate(cate.model =  ~ 1) estimate
  if (all(delta)) {
    outcome_est <- cate(
      cate.model = ~ 1,
      response.model = response.model,
      treatment.model = treatment.model,
      data = data,
      id = id,
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

  ## type = "direct": direct one-step imputation estimator
  ## else: sub-target ("indirect") one-step imputation estimator
  if (!indirect) {
    imputation <- .moi_imputation_context(
      data = data,
      id = id,
      imputation.model = imputation.model,
      imputation.subset = imputation.subset
    )
    branch <- moi_direct(
      data = data,
      id = id,
      delta = delta,
      response = response,
      response.model = response.model,
      treatment.model = treatment.model,
      imputation = imputation,
      nfolds = nfolds,
      silent = silent,
      stratify = stratify,
      mc.cores = mc.cores,
      second.order = second.order
    )
  } else {
    branch <- moi_indirect(
      data = data,
      id = id,
      delta = delta,
      response = response,
      response.model = response.model,
      treatment.model = treatment.model,
      missing.model = missing.model,
      imputation.model = imputation.model,
      imputation.subset = imputation.subset,
      imputation.augmentation.model = imputation.augmentation.model,
      nfolds = nfolds,
      silent = silent,
      stratify = stratify,
      mc.cores = mc.cores,
      second.order = second.order
    )
  }

  ## Set output: relabel per-arm potential outcomes, form the ATE
  ## contrast, and assemble the moi.targeted result.
  ty <- .moi_tilde_y()
  level_labels <- paste0("E[", ty, "(", branch$levels, ")]")
  per_level <- estimate(branch$per_level, labels = level_labels)

  ate_label <- paste0("[", level_labels[1], "] - [", level_labels[2], "]")
  ate <- estimate(per_level, f = cbind(1, -1), labels = ate_label)

  res <- list(
    call = cl,
    estimate = c(per_level, ate),
    levels = branch$levels
  )
  if (isTRUE(return.all)) {
    res$estimate <- merge(per_level, ate, branch$intermediate)
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
