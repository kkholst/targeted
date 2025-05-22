#' @description [learner] generator function for [xgboost::xgboost].
#' @export
#' @param ... Additional arguments to [xgboost::xgboost].
#' @inherit learner_shared
#' @inheritParams xgboost::xgboost
#' @inheritParams xgboost::xgb.cv
# # ' @examples
# TODO examples
learner_xgboost <- function(formula,
                            max_depth = 2L,
                            eta = 1.0,
                            nrounds = 2L,
                            subsample = 1.0,
                            lambda = 1.0,
                            verbose = 0,
                            nfolds = 1L,
                            objective = "reg:squarederror",
                            info = paste("xgboost", objective),
                            learner.args = NULL,
                            ...) {
    args <- c(learner.args, list(formula = formula, info = info))
    estimate.args <- list(
      max_depth = max_depth,
      eta = eta,
      nrounds = nrounds,
      subsample = subsample,
      lambda = lambda,
      verbose = verbose,
      nfolds = nfolds,
      objective = objective
    )
    args$estimate.args <- c(estimate.args, list(...))

    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("xgboost library required")
    }

    args$predict <- function(object, newdata, ...) {
      d <- xgboost::xgb.DMatrix(newdata)
      pr <- predict(object, d, ...)

      if (object$call$objective == "multi:softprob") {
        pr <- matrix(pr, nrow = NROW(d), byrow = TRUE)
      }

      return(pr)
    }
    args$estimate <- function(x, y, nrounds, ...) {
      d <- xgboost::xgb.DMatrix(x, label = y)
      dots <- list(...)
      if (dots$nfolds > 1L) {
        val <- do.call(
          xgboost::xgb.cv,
          c(list(data = d, nrounds = nrounds), dots)
        )
        nrounds <- which.min(val$evaluation_log[[4]])
      }
      dots$nfolds <- NULL # remove nfolds because it causes a warning when
      # passed on to xgboost::xgboost
      res <- do.call(
        xgboost::xgboost,
        c(list(data = d, nrounds = nrounds), dots),
      )
      return(res)
    }

    return(do.call(learner$new, args))
}

#' @export
learner_xgboost_multiclass <- function(formula, ...) {
  return(learner_xgboost(formula, ..., objective = "multi:softprob"))
}

#' @export
learner_xgboost_binary <- function(formula, ...) {
  return(learner_xgboost(formula, ..., objective = "reg:logistic"))
}

#' @export
learner_xgboost_count <- function(formula, ...) {
  return(learner_xgboost(formula, ..., objective = "count:poisson"))
}

#' @export
learner_xgboost_cox <- function(formula, ...) {
  return(learner_xgboost(formula, ..., objective = "survival:cox"))
}
