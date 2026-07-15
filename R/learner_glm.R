# Replace named `...` arguments with symbols of their own name so that a
# rewritten `fit$call` prints compactly (e.g. `weights = weights`) instead of
# embedding the evaluated value (e.g. a full weights vector). Unnamed
# arguments are kept as-is. Purely cosmetic: the symbols only resolve within
# the fitting environment, consistent with how `data`/`family` are stored.
symbolize_dots <- function(dots) {
  nms <- names(dots)
  if (is.null(nms)) return(dots)
  named <- nzchar(nms)
  dots[named] <- lapply(nms[named], as.symbol)
  dots
}



#' @title Construct a learner
#' @name learner_glm
#' @description Constructs a [learner] class object for fitting generalized
#' linear models with [stats::glm] and [MASS::glm.nb]. Negative binomial
#' regression is supported with `family = "nb"` (or alternatively `family =
#' "negbin"`).
#' @param info (character) Optional information to describe the instantiated
#' [learner] object.
#' @param formula (formula) Formula specifying response and design matrix.
#' @param learner.args (list) Additional arguments to
#' [learner$new()][learner].
#' @param ... Additional arguments to [stats::glm] or [MASS::glm.nb].
#' @return [learner] object.
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
      dots <- list(...)
      args <- c(list(formula, data = data), dots)
      fit <- do.call(MASS::glm.nb, args) # use do.call to avoid issues with NSEs

      # store data and named `...` arguments as symbols to avoid dumping the
      # evaluated objects (e.g. the data frame or a weights vector) when the
      # fit is printed
      fit$call <- bquote(
      MASS::glm.nb(.(formula), data = data, ..(symbolize_dots(dots))),
        splice = TRUE
      )
      fit
    }
  } else {
    fitfun <- function(formula, data, family, ...) {
      dots <- list(...)
      args <- c(list(formula, data = data, family = family), dots)
      fit <- do.call(stats::glm, args)

      # store data, family and named `...` arguments as symbols to avoid
      # dumping the evaluated objects (e.g. the data frame or a weights
      # vector) when the fit is printed
      fit$call <- bquote(
      glm(.(formula), data = data, family = family, ..(symbolize_dots(dots))),
        splice = TRUE
      )
      fit
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
  class(mod) <- c("learner_glm", class(mod))
  return(mod)
}
