#' @export
predictor <- function(...) return(ml_model$new(...))

predictor_argument_description <- function(call) {
    ar <- lapply(
      rlang::call_args(call),
      deparse
    )
    ar["info"] <- NULL
    nn <- names(ar)
    desc <- "Arguments:\n"
    for (i in seq_along(nn)) {
      desc <- paste0(desc, "\t", nn[i], " = ", ar[i], "\n")
    }
    return(desc)
  }

#' @export
predictor_glm <- function(formula,
                          info = "glm",
                          family = gaussian(),
                          offset = NULL,
                          ...) {
  args <- c(as.list(environment(), all.names = FALSE), list(...))
    args$estimate <- function(formula, data, family, ...) {
    return(
      stats::glm(formula, data = data, family = family, ...)
    )
  }
  args$predict <- function(object, newdata, ...) {
    return(stats::predict(object, newdata = newdata, type = "response"))
  }
  args$offset <- NULL
  mod <- do.call(ml_model$new, args)
  mod$description <- predictor_argument_description(
    rlang::call_match(defaults = TRUE)
  )
  return(mod)
}

#' @export
predictor_glmnet <- function(formula,
                             info = "glmnet",
                             family = gaussian(),
                             alpha = 1, ## Elastic net (1 is lasso, 0 is L2)
                             lambda = NULL, ## penalty
                             nfolds = 10,
                             ...) {
  est <- function(y, x, ..., nfolds, family) {
    if (nfolds > 1L) {
      res <- glmnet::cv.glmnet(
        x = x, y = y,
        nfolds = nfolds,
        family = family,
        ...
      )
      return(res)
    }
    return(glmnet::glmnet(x = x, y = y, ...)
    )
  }
  pred <- function(object, newdata, ...) {
    res <- predict(object, newx = newdata, family = family, type = "response",
      s = "lambda.min", ...)
    return(res)
  }
  mod <- ml_model$new(
    formula = formula,
    estimate = est,
    predict = pred,
    info = info,
    family = family,
    alpha = alpha,
    lambda = lambda,
    nfolds = nfolds,
    ...
    )
  mod$description <- predictor_argument_description(
    rlang::call_match(defaults = TRUE)
  )
  return(mod)
}

#' @export
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
  mod$description <- predictor_argument_description(
    rlang::call_match(defaults = TRUE)
  )
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
  mod$description <- predictor_argument_description(
    rlang::call_match(defaults = TRUE)
  )
  return(mod)
}

#' @export
predictor_mars <- function(formula,
                           info = "earth::earth",
                           degree = 1,
                           nprune = NULL,
                           glm = NULL,
                             ...) {
  mod <- ml_model$new(formula,
    info = info,
    estimate = function(y, x, ...) {
      return(
        earth::earth(y = y, x = x,
                     degree = degree,
                     nprune = nprune,
                     glm = glm,
                     ...)
      )
    },
    predict = function(object, newdata, type = "response", ...) {
      return(predict(object, newdata=newdata,
                     type = type, ...))
    },
    ...
    )
  mod$description <- predictor_argument_description(
    rlang::call_match(defaults = TRUE)
  )
  return(mod)
}

#' @export
#' @title Superlearner (stacked/ensemble learner)
#' @description This function creates a predictor object (class [ml_model])
#'   from a list of existing [ml_model] objects. When estimating this model a
#'   stacked prediction will be created by weighting together the predictions
#'   of each of the initial models. The weights are learned using
#'   cross-validation.
#' @param model.list List of [ml_model] objects (i.e. [predictor_glm])
#' @param info Optional model description to store in model object
#' @param nfolds Number of folds to use in cross validation
#' @param meta.learner meta.learner function (default non-negative least
#'   squares). Must be a function of the response (nx1 vector), `y`, and the
#'   predictions (nxp matrix), `pred`.
#' @param model.score model scoring method (see [ml_model])
#' @param ... additional argument to `superlearner`
#' @aliases predictor_sl superlearner
#' @references Luedtke & van der Laan (2016) Super-Learning of an Optimal
#'   Dynamic Treatment Rule, The International Journal of Biostatistics.
#' @seealso ml_model predictor_glm predictor_xgboost
#' @examples
#' sim1 <- function(n = 5e2) {
#'    n <- 5e2
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    d <- data.frame(y, x1, x2)
#'    d
#' }
#' d <- sim1()
#'
#' m <- list(
#'   "mean" = predictor_glm(y ~ 1),
#'   "glm" = predictor_glm(y ~ x1 + x2),
#'   "iso" = predictor_isoreg(y ~ x1)
#' )
#'
#' s <- predictor_sl(m, nfolds=10)
#' s$estimate(d)
#' pr <- s$predict(d)
#' if (interactive()) {
#'     plot(y ~ x1, data = d)
#'     points(d$x1, pr, col = 2, cex = 0.5)
#'     lines(cos(x1) + x1 ~ x1, data = d[order(d$x1), ],
#'           lwd = 4, col = lava::Col("darkblue", 0.3))
#' }
#' print(s)
#' ## weights(s)
#' ## score(s)
#'
#' cvres <- summary(s, data=d, nfolds=3, rep=2)
#' cvres
#' ## coef(cvres)
#' ## score(cvres)
predictor_sl <- function(model.list,
                         info = NULL,
                         nfolds = 5L,
                         meta.learner = metalearner_nnls,
                         model.score = mse,
                         ...) {
  args <- c(as.list(environment(), all.names = FALSE), ...)
  if (is.null(info)) {
    info <- "superlearner\n"
    nn <- names(model.list)
    for (i in seq_along(nn)) {
      info <- paste0(info, "\t", nn[i])
      if (i < length(nn)) info <- paste0(info, "\n")
    }
  }
  args$info <- info
  args <- c(args, list(
    estimate = function(data, ...) {
      return(superlearner(data = data, ...))
    },
    predict = function(object, newdata, all.learners = FALSE, ...) {
      pr <- lapply(object$fit, \(x) x$predict(newdata))
      res <- Reduce(cbind, pr)
      if (!is.null(names(model.list)) &&
        length(model.list) == ncol(res)) {
        colnames(res) <- names(model.list)
      } else {
        colnames(res) <- paste0("model", seq_len(length(model.list)))
      }
      if (!all.learners) {
        res <- as.vector(res %*% object$weights)
      }
      return(res)
    }
    ))
  mod <- do.call(ml_model$new, args)
  mod$update(model.list[[1]]$formula)
  cl <- rlang::call_match(defaults = TRUE)
  cl$formula <- lapply(model.list, \(x) x$formula)
  mod$description <- predictor_argument_description(cl)
  attr(mod, "model.score") <- model.score
  class(mod) <- c("predictor_sl", class(mod))
  return(mod)
}

#' @export
weights.predictor_sl <- function(object, ...) {
  return(object$fit$weights)
}

#' @export
score.predictor_sl <- function(x, ...) {
  return(x$fit$model.score)
}

score_sl <- function(response,
                     newdata,
                     object,
                     ...) {
  pr.all <- object$predict(newdata, all.learners = TRUE)
  pr <- object$predict(newdata)
  risk.all <- apply(pr.all, 2, function(x) mse(x, response))
  risk <- mse(response, pr)
  res <- c(risk, risk.all)
  names(res)[1] <- "sl"
  nn <- names(res)
  names(res) <- paste0("score.", nn)
  w <- weights(object)
  names(w) <- paste0("weight.", nn[-1])
  return(c(res, w))
}

#' @export
summary.predictor_sl <- function(object,
                                 data,
                                 nfolds = 5,
                                 rep = 1,
                                 model.score,
                                 ...) {
  res <- cv(list("performance"=object),
            data = data,
            nfolds = nfolds, rep = rep,
            model.score = score_sl
            )

  return(res)
}

#' @export
predictor_xgboost <-
  function(formula,
           max_depth = 2L,
           eta = 1.0,
           nrounds = 2L,
           subsample = 1.0,
           lambda = 1.0,
           verbose = 0,
           nfolds = 1L,
           objective = "reg:squarederror",
           info = paste("xgboost", objective),
           ...) {
    args <- c(as.list(environment(), all.names = FALSE), ...)
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
    mod$description <- predictor_argument_description(
      rlang::call_match(defaults = TRUE)
    )
    return(mod)
  }

#' @export
predictor_xgboost_multiclass <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective="multi:softprob"))
}

#' @export
predictor_xgboost_binary <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective="reg:logistic"))
}

#' @export
predictor_xgboost_count <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective="count:poisson"))
}

#' @export
predictor_xgboost_cox <- function(formula, ...) {
  return(predictor_xgboost(formula, ..., objective="survival:cox"))
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
  mod$description <- predictor_argument_description(
    rlang::call_match(defaults = TRUE)
  )
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


#' ML model
#'
#' Wrapper for ml_model
#' @export
#' @param formula formula
#' @param model model (sl, rf, pf, glm, ...)
#' @param ... additional arguments to model object
#' @details
#' model 'sl' (SuperLearner::SuperLearner)
#' args: SL.library, cvControl, family, method
#' example:
#'
#' model 'grf' (grf::regression_forest)
#' args: num.trees, mtry, sample.weights, sample.fraction, min.node.size, ...
#' example:
#'
#' model 'grf.binary' (grf::probability_forest)
#' args: num.trees, mtry, sample.weights, ...
#' example:
#'
#' model 'glm'
#' args: family, weights, offset, ...
#'
ML <- function(formula, model="glm", ...) {
  model <- tolower(model)
  ## SL / SuperLearner
  if (model == "sl") {
      return(predictor_sl(formula, ...))
  }
  ## grf
  if (model %in% c("grf", "rf", "regression_forest")) {
    return(predictor_grf(formula, ...))
  }
  if (model %in% c("grf.binary", "pf", "probability_forest")) {
    return(predictor_grf_binary(formula, ...))
  }

  ## xgboost
  if (model %in% c(
    "xgboost", "xgb", "xgboost.multiclass",
    "xgboost.binary", "xgboost.count", "xgboost.survival"
  )) {
    obj <- switch(model,
      xgboost.multiclass = "multi:softprob",
      xgboost.binary = "reg:logistic",
      xgboost.survival = "survival:cox",
      xgboost.count = "count:poisson",
      "reg:squarederror"
    )
    return(predictor_xgboost(formula, ..., objective = obj))
  }

  ## GAM
  if (model %in% c("mgcv", "gam")) {
    return(predictor_gam(formula, ...))
  }

  ## glm, default
  m <- predictor_glm(formula, ...)
  return(m)

}
