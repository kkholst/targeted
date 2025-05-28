#' @description Constructs a [learner] class object for isotonic regression with
#' [targeted::isoregw].
#' @export
#' @param ... Additional arguments to [targeted::isoregw].
#' @inherit constructor_shared
#' @inheritParams targeted::isoregw
#' @examples
#' x <- runif(5e3, -5, 5)
#' pr <- lava::expit(-1 + x)
#' y <- rbinom(length(pr), 1, pr)
#' d <- data.frame(y, x)
#'
#' lr <- learner_isoreg(y ~ x)
#' lr$estimate(d)
#' pr_iso <- lr$predict(d)
#'
#' if (interactive()) {
#'   plot(pr ~ x, cex=0.3)
#'   lines(sort(x), pr_iso[order(x)], col="red", type="s")
#' }
learner_isoreg <- function(formula,
                           info = "targeted::isoregw",
                           learner.args = NULL,
                           ...) {
  if (length(all.vars(formula)) != 2) {
    stop("learner_isoreg: expected one outcome and one predictor variable")
  }
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- list(...)

  args$estimate <- function(y, x, ...) isoregw(y = y, x = x, ...)
  args$predict <- function(object, newdata, ...) return(object(newdata))

  return(do.call(learner$new, args))
}
