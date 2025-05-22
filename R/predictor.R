#' @description [ml_model] generator function for [hal9001::fit_hal].
#' @export
#' @param ... Additional arguments to [hal9001::fit_hal].
#' @inherit learner_shared
#' @inheritParams hal9001::fit_hal
predictor_hal <- function(formula,
                          info = "hal9001::fit_hal",
                          smoothness_orders = 0,
                          reduce_basis = NULL,
                          family = "gaussian",
                          ...) {
  est <- function(y, x, ...) {
    return(hal9001::fit_hal(X = x, Y = y, ...))
  }
  pred <- function(fit, newdata, offset = NULL, ...) {
    res <- predict(fit,
      new_data = newdata,
      offset = offset,
      type = "response"
    )
    return(res)
  }
  mod <- ml_model$new(formula = formula,
             estimate = est,
             predict = pred,
             info = info,
             specials = c("weights", "offset"),
             smoothness_orders = smoothness_orders,
             reduce_basis = reduce_basis,
             family = family,
             ...
  )
  return(mod)
}

#' @export
predictor_gam <- function(formula,
                          info = "mgcv::gam",
                          family = gaussian(),
                          select = FALSE,
                          gamma = 1,
                          ...) {
  args <- list(
    formula = formula,
    estimate = function(formula, data, ...) {
      return(mgcv::gam(formula = formula, data = data, ...))
    },
    predict = function(object, newdata) {
      return(stats::predict(object, newdata = newdata, type = "response"))
    },
    family = family,
    select = select,
    gamma = gamma,
    info = info,
    ...
  )
  mod <- do.call(ml_model$new, args)

  return(mod)
}

#' @export
predictor_isoreg <- function(formula,
                             info = "targeted::isoregw",
                             ...) {
  if (length(all.vars(formula)) != 2) {
    stop("predictor_isoreg: expected one outcome and one predictor variable")
  }
  mod <- ml_model$new(formula,
    info = info,
    estimate = function(y, x, ...) return(isoregw(y = y, x = x)),
    predict = function(object, newdata, ...) return(object(newdata)),
    ...
    )

  return(mod)
}


#' @export
predictor_svm <- function(formula,
                          info = "e1071::svm",
                          cost = 1,
                          epsilon = 0.1,
                          kernel = "radial",
                          probability = FALSE,
                          ...) {
  if (probability) {
    formula <- update(formula, factor(.) ~ .)
    pred <- function(object, newdata, ...) {
      pr <- attr(predict(object, newdata, probability = TRUE), "probabilities")
      if (NCOL(pr) == 2L) pr <- pr[, 2]
      return(pr)
    }
  } else {
    pred <- function(object, newdata, ...) {
      return(predict(object, newdata, ...))
    }
  }

  mod <- ml_model$new(formula,
    info = info,
    estimate = function(formula, data, ...) {
      return(
        e1071::svm(formula, data = data, cost = cost, epsilon = epsilon,
          kernel = kernel, probability = probability, ...
        )
      )
    },
    predict = pred,
    ...
  )
  return(mod)
}


#' @export
predictor_xgboost <- function(formula,
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
    estimate.args <- list(max_depth = max_depth, eta = eta,
      nrounds = nrounds, subsample = subsample, lambda = lambda,
      verbose = verbose, nfolds = nfolds, objective = objective
    )
    args$estimate.args <- c(estimate.args, list(...))

    if (!requireNamespace("xgboost")) {
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
    args$estimate <- function(x, y, ..., nfolds, nrounds) {
      d <- xgboost::xgb.DMatrix(x, label = y)
      if (nfolds > 1L) {
        val <- xgboost::xgb.cv(
          data = d,
          nfold = nfolds,
          nrounds = nrounds,
          ...
        )
        nrounds <- which.min(val$evaluation_log[[4]])
      }
      res <- xgboost::xgboost(d, nrounds = nrounds, ...)
      return(res)
    }
    mod <- do.call(ml_model$new, args)
    return(mod)
}

#' @export
predictor_xgboost_multiclass <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective = "multi:softprob"))
}

#' @export
predictor_xgboost_binary <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective = "reg:logistic"))
}

#' @export
predictor_xgboost_count <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective = "count:poisson"))
}

#' @export
predictor_xgboost_cox <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective = "survival:cox"))
}

#' @export
predictor_grf <- function(formula,
                          num.trees = 2000,
                          min.node.size = 5,
                          alpha = 0.05,
                          sample.fraction = 0.5,
                          num.threads = 1,
                          model = "grf::regression_forest",
                          info = model,
                          ...) {
  args <- c(as.list(environment(), all.names = FALSE), ...)
  args$model <- NULL
  est <- utils::getFromNamespace(gsub("^grf::", "", model), "grf")
  pred <- function(object, newdata, ...) {
    return(predict(object, newdata, ...)$predictions)
  }
  args$predict <- pred
  args$estimate <- function(x, y, ...) {
      return(est(X = x, Y = y, ...))
  }
  mod <- do.call(ml_model$new, args)

  return(mod)
}

#' @export
predictor_grf_binary <- function(formula,
                                 ...) {
  mod <- predictor_grf(formula,
    model = "grf::probability_forest",
    ...
  )
  return(mod)
}

#' @export
predictor_nb <- function(formula,
                         info = "Naive Bayes",
                         laplace.smooth = 0,
                         kernel = FALSE,
                         ...) {
  args <- list(
    formula = formula,
    estimate = function(formula, data, ...) {
      return(NB(formula = formula, data = data, ...)
    )
    },
    predict = function(object, newdata, simplify = TRUE, ...) {
      pr <- stats::predict(object, newdata = newdata, ...)
      if (simplify && NCOL(pr) == 2L) pr <- pr[, 2]
      return(pr)
    },
    laplace.smooth = laplace.smooth,
    kernel = kernel,
    info = info,
    specials = c("weights", "offset"),
    ...
  )
  mod <- do.call(ml_model$new, args)
  return(mod)
}

#' ML model
#'
#' Wrapper for ml_model
#' @export
#' @param formula formula
#' @param model model (sl, rf, pf, glm, ...)
#' @param ... additional arguments to model object
ML <- function(formula, model="glm", ...) {
  stop(
    "targeted::ML has been removed in targeted 0.6. ",
    "Please use the targeted::predictor_ functions instead."
  )
}
