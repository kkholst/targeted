#' @description [learner] object constructor for [targeted::isoregw].
#' @export
#' @param ... Additional arguments to [targeted::isoregw].
#' @inherit learner_shared
#' @inheritParams targeted::isoregw
#' @examples
# TODO: examples
learner_isoreg <- function(formula,
                             info = "targeted::isoregw",
                             learner.args = NULL,
                             ...) {
  if (length(all.vars(formula)) != 2) {
    stop("learner_isoreg: expected one outcome and one predictor variable")
  }
  args <- c(learner.args, list(formula = formula, info = info))

  args$estimate <- function(y, x, ...) return(isoregw(y = y, x = x, ...))
  args$predict <- function(object, newdata, ...) return(object(newdata))

  do.call(learner$new, args)
}
