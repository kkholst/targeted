#' @title Construct a learner
#' @param info (character) Optional information to describe the instantiated
#' [learner] object.
#' @param formula (formula) Formula specifying response and design matrix.
#' @param learner.args (list) Additional arguments to
#' [learner$new()][learner].
#' @return [learner] object.
#' @name constructor_shared
NULL


#' @description Constructs a [learner] class object for fitting generalized
#' linear models with [stats::glm] and [MASS::glm.nb]. Negative binomial
#' regression is supported with `family = "nb"` (or alternatively `family =
#' "negbin"`).
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
#' @inherit constructor_shared
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
    trycatch_predictions(stats::predict, args)
  }
  mod <- do.call(learner$new, args)
  class(mod) <- c("learner_glm", class(mod))
  return(mod)
}

# implemented as a utility to be re-used across other learner constructors
# for example, learner_gam also uses stats::predict
# TODO: function needs to be tested, especially for multiclass predictions,
# we need to ensure that the output format is consistent between the original
# function call to the predict function and sapply
trycatch_predictions <- function(pred.fun, args) {
  newdata <- args$newdata

  args_with_data <- args
  args_with_data$newdata <- NULL
  # TODO: also we might want to log when NAs are added
  fallback <- function(i) {
    tryCatch(
      do.call(pred.fun, c(args_with_data, list(newdata = newdata[i, ]))),
      error = \(e) NA
    )
  }

  # TODO: I think we should somehow propagate the error message of
  # do.call(pred_fun, args) to the user. otherwise it might be a bit tricky
  # to understand why the prediction function fails / returns nan values
  preds <- tryCatch(
    do.call(pred.fun, args),
    error = \(e) {
      # TODO: also needs some hardening in case the message field doesn't exist
      # TODO: enable logging logger::log_debug(e$message)
      sapply(
        seq_len(NROW(newdata)),
        fallback
      )
    }
  )

  return(preds)
}
