#' @description [learner] generator function for [e1071::svm]. As shown in the
#' examples, the constructed learner returns predicted class probabilities of
#' class 2 in case of binary classification. A `n times p` matrix, with `n`
#' being the number of observations and `p` the number of classes, is returned
#' for multi-class classification.
#' @export
#' @param ... Additional arguments to [e1071::svm].
#' @inherit learner_shared
#' @inheritParams e1071::svm
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
#' lr <- learner_svm(y ~ x1 + x2)
#' lr$estimate(d)
#' lr$predict(head(d))
#'
#' # binary classification
#' lr <- learner_svm(as.factor(yb) ~ x1 + x2)
#' # alternative to transforming response variable to factor
#' # lr <- learner_svm(yb ~ x1 + x2, type = "C-classification")
#' lr$estimate(d)
#' lr$predict(head(d)) # predict class probabilities of class 2
#' lr$predict(head(d), probability = FALSE) # predict labels
#'
#' # multi-class classification
#' lr <- learner_svm(Species ~ .)
#' lr$estimate(iris)
#' lr$predict(head(iris))
learner_svm <- function(formula,
                        info = "e1071::svm",
                        cost = 1,
                        epsilon = 0.1,
                        kernel = "radial",
                        learner.args = NULL,
                        ...) {
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("e1071 library required")
  }

  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- list(
      cost = cost,
      epsilon = epsilon,
      kernel = kernel,
      probability = TRUE
  )
  args$estimate.args[...names()] <- list(...)

  args$estimate <- function(formula, data, ...) {
    e1071::svm(formula, data = data, ...)
  }
  args$predict <- function(object, newdata, ...) {
    args <- list(object, newdata = newdata, probability = TRUE)
    args[...names()] <- list(...)
    pr <- do.call(predict, args)
    if (args$probability && !is.null(attr(pr, "probabilities"))) {
      pr <- attr(pr, "probabilities")
      if (NCOL(pr) == 2L) pr <- pr[, 2]
    }
    return(pr)
  }

  return(do.call(learner$new, args))
}
