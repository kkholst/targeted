#' @export
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
    pred <- function(object, newdata, ...) {
      d <- xgboost::xgb.DMatrix(newdata)
      return(predict(object, d, ...))
    }
    if (objective == "multi:softprob") {
      pred <- function(object, newdata, ...) {
        d <- xgboost::xgb.DMatrix(newdata)
        val <- predict(object, d, ...)
        return(matrix(val, nrow = NROW(d), byrow = TRUE))
      }
    }
    args$predict <- pred
    args$estimate <- function(x, y, ...) {
      d <- xgboost::xgb.DMatrix(x, label = y)
      if (list(...)$nfolds > 1L) {
        val <- xgboost::xgb.cv(data = d, ...)
        nrounds <- which.min(val$evaluation_log[[4]])
      }
      res <- xgboost::xgboost(d, nrounds = nrounds, ...)
      return(res)
    }
    mod <- do.call(ml_model$new, args)
    return(mod)
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
