#' @description Constructs a [learner] class object for fitting generalized
#' random forest models with [grf::regression_forest] or
#' [grf::probability_forest]. As shown in the examples, the constructed learner
#' returns predicted class probabilities of class 2 in case of binary
#' classification. A `n times p` matrix, with `n` being the number of
#' observations and `p` the number of classes, is returned for multi-class
#' classification.
#' @export
#' @param model (character) grf model to estimate. Usually regression_forest
#' ([grf::regression_forest]) or probability_forest ([grf::probability_forest]).
#' @param ... Additional arguments to `model`
#' @inherit constructor_shared
#' @inheritParams grf::regression_forest
#' @examples
#' n <- 5e2
#' x1 <- rnorm(n, sd = 2)
#' x2 <- rnorm(n)
#' lp <- x2*x1 + cos(x1)
#' yb <- rbinom(n, 1, lava::expit(lp))
#' y <-  lp + rnorm(n, sd = 0.5**.5)
#' d <- data.frame(y, yb, x1, x2)
#'
#' # regression
#' lr <- learner_grf(y ~ x1 + x2)
#' lr$estimate(d)
#' lr$predict(head(d))
#'
#' # binary classification
#' lr <- learner_grf(as.factor(yb) ~ x1 + x2, model = "probability_forest")
#' lr$estimate(d)
#' lr$predict(head(d)) # predict class probabilities of class 2
#'
#' # multi-class classification
#' lr <- learner_grf(Species ~ ., model = "probability_forest")
#' lr$estimate(iris)
#' lr$predict(head(iris))
learner_grf <- function(formula,
                        num.trees = 2000,
                        min.node.size = 5,
                        alpha = 0.05,
                        sample.fraction = 0.5,
                        num.threads = 1,
                        model = "grf::regression_forest",
                        info = model,
                        learner.args = NULL,
                        ...) {
  if (!requireNamespace("grf", quietly = TRUE)) {
      stop("grf library required")
  }
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- c(
    list(
      num.trees = num.trees,
      min.node.size = min.node.size,
      alpha = alpha,
      sample.fraction = sample.fraction,
      num.threads = num.threads
    ),
    list(...)
  )

  est <- getFromNamespace(gsub("^grf::", "", model), "grf")
  args$estimate <- function(y, x, ...) {
    attributes(y) <- NULL
    est(X = x, Y = y, ...)
  }
  args$predict <- function(object, newdata, ...) {
    pr <- predict(object, newdata, ...)$predictions
    if (class(object)[[1]] == "probability_forest" && NCOL(pr) == 2) {
      pr <- pr[, 2]
    }
    return(pr)
  }

  return(do.call(learner$new, args))
}
