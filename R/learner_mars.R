#' @description [learner] generator function for [earth::earth].
#' @param ... Additional arguments to [earth::earth].
#' @inherit learner_shared
#' @inheritParams earth::earth
#' @export
#' @examples
#' # poisson regression
#' n <- 5e2
#' x <- rnorm(n)
#' w <- 50 + rexp(n, rate = 1 / 5)
#' y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
#' d0 <- data.frame(y, x, w)
#'
#' lr <- learner_mars(y ~ x + offset(log(w)), degree = 2,
#'   glm = list(family = poisson())
#' )
#' lr$estimate(d0)
#' lr$predict(data.frame(x = 0, w = c(1, 2)))
learner_mars <- function(formula,
                         info = "earth::earth",
                         degree = 1,
                         nprune = NULL,
                         glm = NULL,
                         learner.args = NULL,
                         ...) {
  if (!requireNamespace("earth", quietly = TRUE)) {
    stop("earth library required")
  }
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- c(
    list(
      degree = degree,
      nprune = nprune,
      glm = glm
    ),
    list(...)
  )
  args$specials <- union(args$specials, c("offset"))

  args$estimate <- function(formula, data, ...) earth::earth(formula, data, ...)
  args$predict <- function(object, newdata, ...) {
    args <- list(object, newdata = newdata, type = "response")
    args[...names()] <- list(...)
    pr <- do.call(predict, args)
    if (ncol(pr) == 1) pr <- as.vector(pr)
    return(pr)
  }

  return(do.call(learner$new, args))
}
