#' @description [learner] model constructor for [xgboost::xgboost].
#' @export
#' @param ... Additional arguments to [xgboost::xgboost].
#' @param max_depth (integer) Maximum depth of a tree.
#' @param eta (numeric) Learning rate.
#' @param subsample (numeric) Subsample ratio of the training instance.
#' @param lambda (numeric) L2 regularization term on weights.
#' @param objective (character) Specify the learning task and the corresponding
#' learning objective. See [xgboost::xgboost] for all available options.
#' @inherit learner_shared
#' @inheritParams xgboost::xgboost
#' @examples
#' # linear regression
#' n  <- 1e3
#' x1 <- rnorm(n, sd = 2)
#' x2 <- rnorm(n)
#' lp <- x2*x1 + cos(x1)
#' yb <- rbinom(n, 1, lava::expit(lp))
#' y <-  lp + rnorm(n, sd = 0.5**.5)
#' d0 <- data.frame(y, yb, x1, x2)
#'
#' lr <- learner_xgboost(y ~ ., nrounds = 5)
#' lr$estimate(d0)
#' lr$predict(head(d0))
#'
#' # multi-class classification
#' d0 <- iris
#' d0$y <- as.numeric(d0$Species)- 1
#'
#' lr <- learner_xgboost(y ~ ., objective = "multi:softprob", num_class = 3)
#' lr$estimate(d0)
#' lr$predict(head(d0))
learner_xgboost <- function(formula,
                            max_depth = 2L,
                            eta = 1.0,
                            nrounds = 2L,
                            subsample = 1.0,
                            lambda = 1,
                            verbose = 0,
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
    args$estimate <- function(x, y, ...) {
      d <- xgboost::xgb.DMatrix(x, label = y)
      res <- do.call(
        xgboost::xgboost,
        c(list(data = d), list(...)),
      )
      return(res)
    }

    return(do.call(learner$new, args))
}
