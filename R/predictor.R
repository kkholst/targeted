#' @title Instantiate a learner
#' @param info (character) Optional information to describe the instantiated
#' [learner] object.
#' @param formula (formula) Formula specifying response and design matrix.
#' @param learner.args (list) Additional arguments to
#' [learner$new()][learner].
#' @return [learner] object.
#' @name learner_shared
NULL


#' @description [learner] generator function for generalized linear models with
#' [stats::glm] and [MASS::glm.nb]. Negative binomial regression is supported
#' with `family = "nb"` (or alternatively `family = "negbin"`).
#' @param ... Additional arguments to [stats::glm] or [MASS::glm.nb].
#' @export
#' @examples
#' n <- 5e2
#' x <- rnorm(n)
#' w <- 50 + rexp(n, rate = 1 / 5)
#' y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
#' d0 <- data.frame(y, x, w)
#'
#' lr <- learner_glm(y ~ x) # linear Gaussian model
#' lr$estimate(d0)
#' coef(lr$fit)
#'
#' # negative binomial regression model with offset (using MASS::glm.nb)
#' lr <- learner_glm(y ~ x + offset(log(w)), family = "nb")
#' lr$estimate(d0)
#' coef(lr$fit)
#' lr$predict(data.frame(x = 1, w = c(1, 5))) # response scale
#' lr$predict(data.frame(x = 1, w = c(1, 5)), type = "link") # link scale
#' @inherit learner_shared
#' @inheritParams stats::glm
learner_glm <- function(formula, info = "glm", family = gaussian(),
  learner.args = NULL, ...) {
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- c(list(family = family), list(...))
  if (is.character(family) && tolower(family) %in% c("nb", "negbin")) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS library required")
    }
    fitfun <- function(formula, data, family, ...) {
      # family is a "pseudo" argument to avoid "multiple local function
      # definitions for ‘fitfun’ with different formal arguments" warnings
      MASS::glm.nb(formula, data = data, ...)
    }
  } else {
    fitfun <- function(formula, data, family, ...) {
      stats::glm(formula, data = data, family = family, ...)
    }
  }

  args$estimate <- fitfun
  args$predict <- function(object, newdata, ...) {
    dots <- list(...)
    if (!("type" %in% names(dots))) dots$type <- "response"
    args <- c(list(object, newdata = newdata), dots)
    do.call(stats::predict, args)
  }
  mod <- do.call(learner$new, args)

  return(mod)
}

#' @description [learner] generator function for [glmnet::cv.glmnet]. Defaults
#' to [glmnet::glmnet] for `nfolds = 1`.
#' @export
#' @inherit learner_shared
#' @inheritParams glmnet::glmnet
#' @inheritParams glmnet::cv.glmnet
learner_glmnet <- function(formula,
                             info = "glmnet",
                             family = gaussian(),
                             alpha = 1, ## Elastic net (1 is lasso, 0 is L2)
                             lambda = NULL, ## penalty
                             nfolds = 10,
                             learner.args = NULL,
                             ...) {
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- list(alpha = alpha, lambda = lambda, nfolds = nfolds,
    family = family)
  args$estimate <- function(y, x, ..., nfolds, family) {
    if (nfolds > 1L) {
      return(glmnet::cv.glmnet(
        x = x, y = y, nfolds = nfolds, family = family, ...
      ))
    }
    return(glmnet::glmnet(x = x, y = y, ...))
  }
  args$predict <- function(object, newdata, ...) {
    args <- c(object, list(newx = newdata), list(...))
    args[c("type", "s")] <- list(type = "response", s = "lambda.min")
    return(do.call(predict, args))
  }
  return(do.call(learner$new, args))
}

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
        earth::earth(y = y, x = x, degree = degree, nprune = nprune, glm = glm,
          ...
        )
      )
    },
    predict = function(object, newdata, type = "response", ...) {
      return(predict(object, newdata = newdata, type = type, ...))
    },
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


#' @description [learner] generator function for [superlearner]
#' @export
#' @inherit learner_shared
#' @inheritParams superlearner
#' @param ... Additional arguments to [superlearner]
#' @examples
#' sim1 <- function(n = 5e2) {
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    data.frame(y, x1, x2)
#' }
#' d <- sim1()
#'
#' m <- list(
#'   "mean" = learner_glm(y ~ 1),
#'   "glm" = learner_glm(y ~ x1 + x2),
#'   "iso" = predictor_isoreg(y ~ x1)
#' )
#'
#' s <- predictor_sl(m, nfolds = 10)
#' s$estimate(d)
#' pr <- s$predict(d)
#' if (interactive()) {
#'     plot(y ~ x1, data = d)
#'     points(d$x1, pr, col = 2, cex = 0.5)
#'     lines(cos(x1) + x1 ~ x1, data = d[order(d$x1), ],
#'           lwd = 4, col = lava::Col("darkblue", 0.3))
#' }
#' print(s)
#' # weights(s$fit)
#' # score(s$fit)
#'
#' cvres <- summary(s, data = d, nfolds = 3, rep = 2)
#' cvres
#' # coef(cvres)
#' # score(cvres)
predictor_sl <- function(learners,
                         info = NULL,
                         nfolds = 5L,
                         meta.learner = metalearner_nnls,
                         model.score = mse,
                         learner.args = NULL,
                         ...) {

  if (is.null(info)) {
    info <- "superlearner\n"
    nn <- names(learners)
    for (i in seq_along(nn)) {
      info <- paste0(info, "\t", nn[i])
      if (i < length(nn)) info <- paste0(info, "\n")
    }
  }
  args <- c(learner.args, list(info = info))
  estimate.args <- list(learners = learners, nfolds = nfolds,
    meta.learner = meta.learner, model.score = model.score
  )
  args$estimate.args <- c(estimate.args, list(...))
  args$estimate <- function(data, ...) superlearner(data = data, ...)
  args$predict <- function(object, newdata, ...) predict(object, newdata, ...)

  mod <- do.call(learner$new, args)

  # duplicate check from superlearner to catch error during instantiation
  # of a learner instead in the estimate method call
  if (length(unique(lapply(learners, \(m) all.vars(m$formula)[[1]]))) > 1) {
    stop("All learners must have the same response variable.")
  }
  mod$update(learners[[1]]$formula) # TODO: not clean but will be fixed later
  # because it requires changes to the learner R6 class and the cv function

  attr(mod, "model.score") <- model.score
  class(mod) <- c("predictor_sl", class(mod))
  return(mod)
}

score_sl <- function(response,
                     newdata,
                     object,
                     model.score = mse,
                     ...) {
  pr.all <- object$predict(newdata, all.learners = TRUE)
  pr <- object$predict(newdata)
  risk.all <- apply(pr.all, 2, function(x) model.score(x, response)[1])
  risk <- model.score(response, pr)[1]
  nam <- names(risk)
  if (is.null(nam)) nam <- "score"
  nam <- paste0(nam, ".")
  res <- c(risk, risk.all)
  names(res)[1] <- "sl"
  nn <- names(res)
  names(res) <- paste0(nam, nn)
  w <- c(NA, weights(object$fit))
  names(w) <- paste0("weight.", nn)
  return(c(res, w))
}

#' @export
summary.predictor_sl <- function(object,
                                 data,
                                 nfolds = 5,
                                 rep = 1,
                                 model.score = mse,
                                 ...) {
  res <- cv(list("performance"=object),
            data = data,
            nfolds = nfolds, rep = rep,
            model.score = function(...) score_sl(..., model.score = model.score)
            )
  nam <- dimnames(res$cv)
  nam <- nam[[length(nam)]]
  st <- strsplit(c(nam[1], tail(nam, 1)), "\\.")
  type <- unlist(lapply(st, \(x) x[1]))
  n <- length(nam)/2
  nam <- gsub(paste0(type[1], "\\."), "", nam[seq_len(n)])
  score <- res$cv[, , , 1:n, drop=FALSE]
  weight <- res$cv[, , , (n+1):(2*n), drop=FALSE]
  cvs <- abind::abind(score, weight, along=3)
  dimnames(cvs)[[4]] <- nam
  dimnames(cvs)[[3]] <- type
  cvs <- aperm(cvs, c(1, 2, 4, 3))
  res$names <- nam
  res$cv <- cvs
  res$call <- NULL
  class(res) <- c("summary.predictor_sl", "cross_validated")
  return(res)
}

#' @export
print.summary.predictor_sl <- function(x, digits=5, ...) {
  res <- round(summary.cross_validated(x)*1e5, digits=0) / 1e5
  cat(x$fold, "-fold cross-validation", sep="")
  if (x$rep > 1) cat(" with ", x$rep, " repetitions", sep="")
  cat("\n\n")
  cli::cli_h3(dimnames(res)[[3]][2])
  print(res[, , 2], na.print="-")
  cli::cli_h3(dimnames(res)[[3]][1])
  print(res[, , 1], na.print="-")
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
