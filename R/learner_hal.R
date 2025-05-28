#' @description Constructs a [learner] class object for fitting a highly
#' adaptive lasso model with [hal9001::fit_hal].
#' @export
#' @param ... Additional arguments to [hal9001::fit_hal].
#' @inherit constructor_shared
#' @inheritParams hal9001::fit_hal
#' @examples
#' \dontrun{
#' n <- 5e2
#' x1 <- rnorm(n, sd = 2)
#' x2 <- rnorm(n)
#' y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#' d <- data.frame(y, x1, x2)
#' lr <- learner_hal(y ~ x1 + x2, smoothness_orders = 0.5, reduce_basis = 1)
#' lr$estimate(d)
#' lr$predict(data.frame(x1 = 0, x2 = c(-1, 1)))
#' }
learner_hal <- function(formula,
                        info = "hal9001::fit_hal",
                        smoothness_orders = 0,
                        reduce_basis = NULL,
                        family = "gaussian",
                        learner.args = NULL,
                        ...) {
  args <- c(learner.args, list(formula = formula, info = info))
  # ensure that offset is always added as a special to be handled by
  # targeted::design inside targeted::learner
  args$specials <- union(args$specials, c("weights", "offset"))
  args$estimate.args <- c(
    list(
      smoothness_orders = smoothness_orders,
      reduce_basis = reduce_basis,
      family = family
    ),
    list(...)
  )
  args$estimate <- function(y, x, ...) hal9001::fit_hal(X = x, Y = y, ...)
  args$predict <- function(fit, newdata, ...) {
    args <- list(fit, new_data = newdata, type = "response")
    args[...names()] <- list(...)
    do.call(predict, args)
  }
  return(do.call(learner$new, args))
}
