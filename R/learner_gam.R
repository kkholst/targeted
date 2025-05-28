#' @description Constructs [learner] class object for fitting generalized
#' additive models with [mgcv::gam].
#' @export
#' @param ... Additional arguments to [mgcv::gam].
#' @inherit constructor_shared
#' @inheritParams mgcv::gam
#' @examples
#' n <- 5e2
#' x1 <- rnorm(n, sd = 2)
#' x2 <- rnorm(n)
#' y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#' d0 <- data.frame(y, x1, x2)
#'
#' lr <- learner_gam(y ~ s(x1) + x2)
#' lr$estimate(d0)
#' if (interactive()) {
#'   plot(lr$fit)
#' }
learner_gam <- function(formula,
                        info = "mgcv::gam",
                        family = gaussian(),
                        select = FALSE,
                        gamma = 1,
                        learner.args = NULL,
                        ...) {
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- c(
    list(
      family = family,
      select = select,
      gamma = gamma
    ),
    list(...)
  )
  args$estimate <- function(formula, data, ...) {
    return(mgcv::gam(formula = formula, data = data, ...))
  }
  args$predict <- function(object, newdata, ...) {
    args <- list(object, newdata = newdata, type = "response")
    args[...names()] <- list(...)
    pr <- do.call(stats::predict, args)
    return(as.vector(pr))
  }

  return(do.call(learner$new, args))
}
