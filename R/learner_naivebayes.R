#' @description [learner] object constructor for [naivebayes]. As shown in
#' the examples, the constructed learner returns predicted class probabilities
#' of class 2 in case of binary classification. A `n times p` matrix, with `n`
#' being the number of observations and `p` the number of classes, is returned
#' for multi-class classification.
#' @export
#' @param ... Additional arguments to [naivebayes].
#' @inherit learner_shared
#' @inheritParams naivebayes
#' @examples
#' n <- 5e2
#' x1 <- rnorm(n, sd = 2)
#' x2 <- rnorm(n)
#' y <- rbinom(n, 1, lava::expit(x2*x1 + cos(x1)))
#' d <- data.frame(y, x1, x2)
#'
#' # binary classification
#' lr <- learner_naivebayes(y ~ x1 + x2)
#' lr$estimate(d)
#' lr$predict(head(d))
#'
#' # multi-class classification
#' lr <- learner_naivebayes(Species ~ .)
#' lr$estimate(iris)
#' lr$predict(head(iris))
learner_naivebayes <- function(formula,
                       info = "Naive Bayes",
                       laplace.smooth = 0,
                       kernel = FALSE,
                       learner.args = NULL,
                       ...) {
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- c(
    list(
      laplace.smooth = laplace.smooth,
      kernel = kernel
    ),
    list(...)
  )
  args$specials <- union(args$specials, c("weights"))

  args$estimate <- function(formula, data, ...) {
    naivebayes(formula = formula, data = data, ...)
  }
  args$predict <- function(object, newdata, ...) {
    pr <- stats::predict(object, newdata = newdata, ...)
    if (NCOL(pr) == 2L) pr <- pr[, 2]
    return(pr)
  }

  return(do.call(learner$new, args))
}
