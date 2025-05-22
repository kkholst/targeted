#' @export
#' @description [learner] generator function for [glmnet::cv.glmnet].
#' Predictions are returned for the value of `lambda` that gives minimum `cvm`.
#' That is, [glmnet::predict.cv.glmnet] is called with `s = "lambda.min"`.
#' @inherit learner_shared
#' @inheritParams glmnet::cv.glmnet
#' @inheritParams glmnet::glmnet # required to document family and alpha args
#' @examples
#' # continuous outcome
#' n <- 5e2
#' x1 <- rnorm(n, sd = 2)
#' x2 <- rnorm(n)
#' lp <- x1 + x2*x1 + cos(x1)
#' y <- rnorm(n, lp, sd = 2)
#' d0 <- data.frame(y, x1, x2)
#'
#' lr <- learner_glmnet_cv(y ~ x1 + x2)
#' lr$estimate(d0, nfolds = 3)
#' lr$predict(data.frame(x1 = c(0, 1), x2 = 1))
#'
#' # count outcome with different exposure time
#' w <- 50 + rexp(n, rate = 1 / 5)
#' y <- rpois(n, exp(0.5 * x1 - 1 * x2 + log(w)) * rgamma(n, 1 / 2, 1 / 2))
#' d0 <- data.frame(y, x1, x2, w)
#'
#' lr <- learner_glmnet_cv(y ~ x1 + x2 + offset(log(w)), family = "poisson")
#' lr$estimate(d0, nfolds = 3)
#' lr$predict(data.frame(x1 = 1, x2 = 1, w = c(1, 5)))
learner_glmnet_cv <- function(formula,
                              info = "glmnet::cv.glmnet",
                              family = gaussian(),
                              lambda = NULL,
                              alpha = 1, ## Elastic net (1 is lasso, 0 is L2)
                              nfolds = 10,
                              learner.args = NULL,
                              ...) {
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- c(
    list(
      alpha = alpha,
      lambda = lambda,
      nfolds = nfolds,
      family = family),
    list(...)
  )
  # offset always needs to be a special such that targeted::design handles the
  # offset correctly inside targeted::learner
  args$specials <- union(args$specials, c("offset"))

  args$estimate <- function(y, x, ...) glmnet::cv.glmnet(x = x, y = y, ...)
  args$predict <- function(object, newdata, ...) {
    args <- c(list(object, newx = newdata, type = "response", s = "lambda.min"))
    args[...names()] <- list(...)
    if ("offset" %in% (.names <- names(args))) {
      names(args)[.names == "offset"] <- "newoffset"
    }

    # predictions are by default a matrix. convert to vector for
    # compatibility with other learner_ functions
    pr <- do.call(predict, args)
    if (ncol(pr) == 1) pr <- as.vector(pr) #
    return(pr)
  }
  return(do.call(learner$new, args))
}
