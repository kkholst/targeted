#' @description [learner] generator function for [earth::earth].
#' @param ... Additional arguments to [earth::earth].
#' @inherit learner_shared
#' @inheritParams earth::earth
#' @export
learner_mars <- function(formula,
                           info = "earth::earth",
                           degree = 1,
                           nprune = NULL,
                           glm = NULL,
                           learner.args = NULL,
                           ...) {
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
