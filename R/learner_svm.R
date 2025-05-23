#' @description [learner] generator function for [e1071::svm].
#' @export
#' @param ... Additional arguments to [e1071::svm].
#' @inherit learner_shared
#' @inheritParams e1071::svm
#' @examples
#' print(1)
# TODO: examples
learner_svm <- function(formula,
                        info = "e1071::svm",
                        cost = 1,
                        epsilon = 0.1,
                        kernel = "radial",
                        probability = FALSE,
                        learner.args = list(predict.args = list(
                          probability = probability
                        )),
                        ...) {
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("e1071 library required")
  }

  # TODO: do we really need this transformation?
  if (probability) formula <- update(formula, factor(.) ~ .)
  args <- c(learner.args, list(formula = formula, info = info))

  args$estimate.args <- c(
    list(
      cost = cost,
      epsilon = epsilon,
      kernel = kernel,
      probability = probability
    ),
    list(...)
  )

  args$estimate <- function(formula, data, ...) {
    e1071::svm(formula, data = data, ...)
  }
  args$predict <- function(object, newdata, ...) {
    args <- c(list(object, newdata = newdata), ...)
    pr <- do.call(predict, args)
    if (args$probability) {
      pr <- attr(predict(object, newdata, probability = TRUE), "probabilities")
      if (NCOL(pr) == 2L) pr <- pr[, 2] # TODO: why prob of second level?
    }
    return(pr)
  }

  return(do.call(learner$new, args))
}
