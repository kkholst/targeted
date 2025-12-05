#' @description Constructs a [learner] class object for [xgboost::xgboost].
#' @param ... Additional arguments to [xgboost::xgboost].
#' @param max_depth (integer) Maximum depth of a tree.
#' @param learning_rate (numeric) Learning rate.
#' @param subsample (numeric) Subsample ratio of the training instance.
#' @param reg_lambda (numeric) L2 regularization term on weights.
#' @param objective (character) Specify the learning task and the corresponding
#' learning objective. See [xgboost::xgboost] for all available options.
#' @inherit constructor_shared
#' @inheritParams xgboost::xgboost
#' @export
#' @examples
#' n  <- 1e3
#' x1 <- rnorm(n, sd = 2)
#' x2 <- rnorm(n)
#' lp <- x2*x1 + cos(x1)
#' yb <- rbinom(n, 1, lava::expit(lp))
#' y <-  lp + rnorm(n, sd = 0.5**.5)
#' d0 <- data.frame(y, yb, x1, x2)
#'
#' # regression
#' lr <- learner_xgboost(y ~ x1 + x2, nrounds = 5)
#' lr$estimate(d0)
#' lr$predict(head(d0))
#'
#' # binary classification
#' lr <- learner_xgboost(yb ~ x1 + x2, nrounds = 5,
#'  objective = "binary:logistic"
#' )
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
                            learning_rate = 1.0,
                            nrounds = 2L,
                            subsample = 1.0,
                            reg_lambda = 1,
                            objective = "reg:squarederror",
                            info = paste("xgboost", objective),
                            learner.args = NULL,
                            ...) {
    args <- c(learner.args, list(formula = formula, info = info))
    estimate.args <- list(
      max_depth = max_depth,
      learning_rate = learning_rate,
      nrounds = nrounds,
      subsample = subsample,
      reg_lambda = reg_lambda,
      objective = objective
    )
    args$estimate.args <- c(estimate.args, list(...))

    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("xgboost library required")
    }

    args$predict <- function(object, newdata, ...) {
      d <- xgboost::xgb.DMatrix(newdata)
      pr <- predict(object, d, ...)
      ## if (attributes(object)$call$params$objective == "multi:softprob") {
      ## pr <- matrix(pr, nrow = NROW(newdata), byrow = TRUE)
      ## }
      return(pr)
    }
    args$estimate <- function(x, y, ...) {
      params <- list(...)
      par1 <- intersect(formalArgs(xgboost::xgb.params), names(params))
      xgb_params <- params[par1]
      xgb_train_args <- params
      xgb_train_args[par1] <- NULL
      d <- xgboost::xgb.DMatrix(x, label = y)
      res <- do.call(
        xgboost::xgb.train,
        c(list(data = d), xgb_train_args, list(params = xgb_params)),
        )
      return(res)
    }
    return(do.call(learner$new, args))
}
