#' @title Instantiate a learner
#' @param info (character) Optional information to describe the instantiated
#' [learner] object.
#' @param formula (formula) Formula specifying response and design matrix.
#' @param learner.args (list) Additional arguments to
#' [learner$new()][learner].
#' @return [learner] object.
#' @name learner_shared
NULL


#' @description [learner] generator function for generalized linear models with
#' [stats::glm] and [MASS::glm.nb]. Negative binomial regression is supported
#' with `family = "nb"` (or alternatively `family = "negbin"`).
#' @param ... Additional arguments to [stats::glm] or [MASS::glm.nb].
#' @export
#' @examples
#' n <- 5e2
#' x <- rnorm(n)
#' w <- 50 + rexp(n, rate = 1 / 5)
#' y <- rpois(n, exp(2 + 0.5 * x + log(w)) * rgamma(n, 1 / 2, 1 / 2))
#' d0 <- data.frame(y, x, w)
#'
#' lr <- learner_glm(y ~ x) # linear Gaussian model
#' lr$estimate(d0)
#' coef(lr$fit)
#'
#' # negative binomial regression model with offset (using MASS::glm.nb)
#' lr <- learner_glm(y ~ x + offset(log(w)), family = "nb")
#' lr$estimate(d0)
#' coef(lr$fit)
#' lr$predict(data.frame(x = 1, w = c(1, 5))) # response scale
#' lr$predict(data.frame(x = 1, w = c(1, 5)), type = "link") # link scale
#' @inherit learner_shared
#' @inheritParams stats::glm
learner_glm <- function(formula, info = "glm", family = gaussian(),
  learner.args = NULL, ...) {
  args <- c(learner.args, list(formula = formula, info = info))
  args$estimate.args <- c(list(family = family), list(...))
  if (is.character(family) && tolower(family) %in% c("nb", "negbin")) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS library required")
    }
    fitfun <- function(formula, data, family, ...) {
      # family is a "pseudo" argument to avoid "multiple local function
      # definitions for ‘fitfun’ with different formal arguments" warnings
      MASS::glm.nb(formula, data = data, ...)
    }
  } else {
    fitfun <- function(formula, data, family, ...) {
      stats::glm(formula, data = data, family = family, ...)
    }
  }

  args$estimate <- fitfun
  args$predict <- function(object, newdata, ...) {
    dots <- list(...)
    if (!("type" %in% names(dots))) dots$type <- "response"
    args <- c(list(object, newdata = newdata), dots)
    do.call(stats::predict, args)
  }
  mod <- do.call(learner$new, args)

  return(mod)
}
