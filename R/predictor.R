##' @export
predictor <- function(...) {
  ml_model$new(...)
}


update_call_predictor <- function(call.object, call.remove=NULL, ...) {
  call.object[[1]] <- quote(ml_model$new)
  dots <- list(...)
  nn <- names(dots)
  for (i in seq_along(dots)) {
    call.object[[nn[i]]] <- dots[[i]]
  }
  if (!is.null(call.remove)) {
    call.object[call.remove] <- NULL
  }
  return(call.object)
}

##' @export
predictor_glm <- function(formula,
                          info = "glm",
                          offset = NULL,
                          ...) {
  m <- ml_model$new(
    add_offset(formula, offset),
    info = info,
    ...,
    estimate = function(formula, data, ...) {
      stats::glm(formula,
        data = data,
        ...
      )
    },
    predict = function(object, newdata, ...) {
      stats::predict(object,
        newdata = newdata,
        type = "response"
      )
    }
    )
  return(m)
}

predictor_svm <- function(formula,
                          info = "e1071::svm",
                          cost = 1.0,
                          ...) {
  # TODO
  }


##' @export
predictor_glmnet <- function(formula,
                             info = "glmnet",
                             family = gaussian(),
                             alpha = 1, ## Elastic net (1 is lasso, 0 is L2)
                             lambda = NULL, ## penalty
                             nfolds = 10,
                             ...) {
  est <- function(y, x, ..., nfolds) {
    if (nfolds > 1L) {
      res <- glmnet::cv.glmnet(
        x = x, y = y,
        nfolds = nfolds,
        ...
      )
      return(res)
    }
    glmnet::glmnet(
      x = x, y = y,
      ...
    )
  }
  cl <- update_call_predictor(
    rlang::call_match(
      fn = predictor_glmnet,
      defaults = TRUE
      ),
    estimate = est,
    predict = function(object, newdata, ...) {
      predict(object,
        newx = newdata,
        type = "response",
        s = "lambda.min",
        ...
      )
    }
  )
  eval(cl)
}



##' @export
predictor_hal <- function(formula,
                          info = "hal9001::fit_hal",
                          smoothness_orders = 0,
                          reduce_basis = NULL,
                          family = gaussian(),
                          ...) {
  est <- function(y, x, ...) {
    hal9001::fit_hal(
      X = x,
      Y = y,
      ...
    )
  }
  pred <- function(fit, newdata, offset = NULL, ...) {
    res <- predict(fit,
      new_data = newdata,
      offset = offset,
      type = "response"
      )
    return(res)
  }
  cl <- update_call_predictor(
    rlang::call_match(
      fn = predictor_hal,
      defaults = TRUE
    ),
    estimate = est,
    predict = pred,
    specials = c("weights", "offset")
  )
  eval(cl)
}

##' @export
predictor_gam <- function(formula,
                          info = "mgcv::gam",
                          family = gaussian(),
                          select = FALSE,
                          gamma = 1,
                          ...) {

   cl <- update_call_predictor(
     rlang::call_match(
       fn = predictor_gam,
       defaults = TRUE
     ),
     estimate = function(formula, data, ...) {
       mgcv::gam(
         formula = formula,
         data = data,
         ...
         )
     },
     predict = function(object, newdata) {
       stats::predict(object, newdata = newdata, type = "response")
     }
   )
  eval(cl)
}

##' @export
predictor_isoreg <- function(formula,
                             info = "targeted::isoregw",
                             ...) {
  if (length(all.vars(formula)) != 2) {
    stop("predictor_isoreg: expected one outcome and one predictor variable")
  }
  ml_model$new(formula,
    info = info,
    estimate = function(y, x, ...) {
      isoregw(y = y, x = x)
    },
    predict = function(object, newdata, ...) {
      object(newdata)
    }
  )
}

##' @export
##' @title Superlearner (stacked/ensemble learner)
##' @description This function creates a predictor object (class [ml_model])
##'   from a list of existing [ml_model] objects. When estimating this model a
##'   stacked prediction will be created by weighting together the predictions
##'   of each of the initial models. The weights are learned using
##'   cross-validation.
##' @param model.list List of [ml_model] objects (i.e. [predictor_glm])
##' @param info Optional model description to store in model object
##' @param nfolds Number of folds to use in cross validation
##' @param meta.learner meta.learner function (default non-negative least
##'   squares). Must be a function of the response (nx1 vector), `y`, and the
##'   predictions (nxp matrix), `pred`.
##' @param model.score model scoring method (see [ml_model])
##' @param ... additional argument to `superlearner`
##' @aliases predictor_sl superlearner
##' @references Luedtke & van der Laan (2016) Super-Learning of an Optimal
##'   Dynamic Treatment Rule, The International Journal of Biostatistics.
##' @seealso ml_model predictor_glm predictor_xgboost
##' @examples
##' sim1 <- function(n = 5e2) {
##'    n <- 5e2
##'    x1 <- rnorm(n, sd = 2)
##'    x2 <- rnorm(n)
##'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
##'    d <- data.frame(y, x1, x2)
##'    d
##' }
##' d <- sim1() |> mets::dsort(~x1)
##'
##' m <- list(
##'   "mean" = predictor_glm(y ~ 1),
##'   "glm" = predictor_glm(y ~ x1 + x2),
##'   "iso" = predictor_isoreg(y ~ x1)
##' )
##'
##' s <- predictor_sl(m, nfolds=10)
##' s$estimate(d)
##' pr <- s$predict(d)
##' if (interactive()) {
##'     plot(y ~ x1, data = d)
##'     points(d$x1, pr, col = 2, cex = 0.5)
##'     lines(cos(x1) + x1 ~ x1, data = d,
##'           lwd = 4, col = lava::Col("darkblue", 0.3))
##' }
##' print(s)
##' ## weights(s)
##' ## score(s)
##'
##' cvres <- summary(s, data=d, nfolds=3, rep=2)
##' cvres
##' ## coef(cvres)
##' ## score(cvres)
predictor_sl <- function(model.list,
                         info = NULL,
                         nfolds = 5L,
                         meta.learner = NULL,
                         model.score = mse,
                         ...) {
  if (is.null(info)) {
    info <- "superlearner\n"
    nn <- names(model.list)
    for (i in seq_along(nn)) {
      info <- paste0(info, "\t", nn[i])
      if (i < length(nn)) info <- paste0(info, "\n")
    }
  }
  cl <- update_call_predictor(
    rlang::call_match(
      fn = predictor_sl,
      defaults = TRUE
      ),
    estimate = function(data, ...) {
      superlearner(
        data = data,
        ...
      )
    },
    predict = function(object, newdata, all.learners = FALSE, ...) {
      pr <- lapply(object$fit, \(x) x$predict(newdata))
      res <- Reduce(cbind, pr)
      if (!is.null(names(model.list)) &&
        length(model.list) == ncol(res)) {
        colnames(res) <- names(model.list)
      } else {
        colnames(res) <- paste0("model", 1:length(model.list))
      }
      if (!all.learners) {
        res <- as.vector(res %*% object$weights)
      }
      return(res)
    },
    info = info
  )
  sl <- eval(cl)
  sl$update(model.list[[1]]$formula)
  class(sl) <- c("predictor_sl", class(sl))
  return(sl)
}

##' @export
weights.predictor_sl <- function(object, ...) {
  return(object$fit$weights)
}

##' @export
score.predictor_sl <- function(x, ...) {
  return(x$fit$model.score)
}


score_sl <- function(response, prediction, weights, object, newdata, ...) {
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
    c(res, w)
}

##' @export
summary.predictor_sl <- function(object, data, nfolds = 5, rep = 1, ...) {
  res <- cv(list("performance"=object),
      data = data,
      nfolds = nfolds, rep = rep,
      model.score = score_sl
      )
  res
}

##' @export
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
    if (!requireNamespace("xgboost")) {
      stop("xgboost library required")
    }
    pred <- function(object, newdata, ...) {
      d <- xgboost::xgb.DMatrix(newdata)
      predict(object, d, ...)
    }
    if (objective == "multi:softprob") {
      pred <- function(object, newdata, ...) {
        d <- xgboost::xgb.DMatrix(newdata)
        val <- predict(object, d, ...)
        matrix(val, nrow = NROW(d), byrow = TRUE)
      }
    }
    est <- function(x, y, ..., nfolds, nrounds) {
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
      xgboost::xgboost(
        d,
        nrounds = nrounds,
        ...
      )
    }
    cl <- update_call_predictor(
      rlang::call_match(
        fn = predictor_xgboost,
        defaults = TRUE
      ),
      estimate = est,
      predict = pred
    )
    eval(cl)
  }

##' @export
predictor_xgboost_multiclass <- function(formula, ...) {
  predictor_xgboost(formula, ..., objective="multi:softprob")
}

##' @export
predictor_xgboost_binary <- function(formula, ...) {
  predictor_xgboost(formula, ..., objective="reg:logistic")
}

##' @export
predictor_xgboost_count <- function(formula, ...) {
  predictor_xgboost(formula, ..., objective="count:poisson")
}

##' @export
predictor_xgboost_cox <- function(formula, ...) {
  predictor_xgboost(formula, ..., objective="survival:cox")
}

##' @export
predictor_grf <- function(formula,
                          num.trees = 2000,
                          min.node.size = 5,
                          alpha = 0.05,
                          sample.fraction = 0.5,
                          num.threads = 1,
                          model = "grf::regression_forest",
                          info = model,
                          ...) {
  est <- utils::getFromNamespace(gsub("^grf::", "", model), "grf")
  pred <- function(object, newdata, ...) {
    predict(object, newdata, ...)$predictions
  }
  cl <- update_call_predictor(
    rlang::call_match(
      fn = predictor_grf,
      defaults = TRUE
      ),
    call.remove = "model",
    estimate = function(x, y, ...) {
      est(X = x, Y = y, ...)
    },
    predict = pred
  )
  eval(cl)
}


##' @export
predictor_grf_binary <- function(formula,
                                 ...) {
  predictor_grf(formula,
    model = "grf::probability_forest",
    ...
    )
}


##' ML model
##'
##' Wrapper for ml_model
##' @export
##' @param formula formula
##' @param model model (sl, rf, pf, glm, ...)
##' @param ... additional arguments to model object
##' @details
##' model 'sl' (SuperLearner::SuperLearner)
##' args: SL.library, cvControl, family, method
##' example:
##'
##' model 'grf' (grf::regression_forest)
##' args: num.trees, mtry, sample.weights, sample.fraction, min.node.size, ...
##' example:
##'
##' model 'grf.binary' (grf::probability_forest)
##' args: num.trees, mtry, sample.weights, ...
##' example:
##'
##' model 'glm'
##' args: family, weights, offset, ...
##'
ML <- function(formula, model="glm", ...) {
  model <- tolower(model)
  dots <- list(...)
  addargs <- function(..., dots, args = list()) {
      for (p in names(args)) {
          if (!(p %in% names(dots))) dots[p] <- args[[p]]
      }
      c(list(...), dots)
  }

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
