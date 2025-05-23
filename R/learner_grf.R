#' @description [learner] object constructor for generalized random forest.
#' @export
#' @param model (character) grf model to estimate. Can be any of
#' [grf::regression_forest] or [grf::probability_forest].
#' @param ... Additional arguments to `model`
#' @inherit learner_shared
#' @inheritParams grf::regression_forest
#' @examples
#' print(1)
#' # TODO
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
  args$estimate <- function(y, x, ...) est(X = x, Y = y, ...)
  args$predict <- function(object, newdata, ...) {
    predict(object, newdata, ...)$predictions
  }

  return(do.call(learner$new, args))
}
