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
