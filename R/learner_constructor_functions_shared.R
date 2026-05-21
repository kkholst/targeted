#' @title Construct a learner
#' @param info (character) Optional information to describe the instantiated
#' [learner] object.
#' @param formula (formula) Formula specifying response and design matrix.
#' @param learner.args (list) Additional arguments to
#' [learner$new()][learner].
#' @return [learner] object.
#' @name constructor_shared
NULL

# implemented as a utility to be re-used across other learner constructors
# for example, learner_gam also uses stats::predict
# TODO: function needs to be tested, especially for multiclass predictions,
# we need to ensure that the output format is consistent between the original
# function call to the predict function and sapply
standardize_learner_predictions <- function(pred.fun, args, model.info) {
  if (is.null(model.info)) model.info <- "learner"
  newdata <- args$newdata
  args_without_data <- args
  args_without_data$newdata <- NULL
  warn <- err <- NULL
  fallback <- function(i) {
    tryCatch(
      do.call(pred.fun, c(
        args_without_data,
        list(newdata = newdata[i, ,drop = FALSE]))
      ),
      error = \(e) {
        err <<- conditionMessage(e)
        return(NA)
      }
    )
  }

  preds <- withCallingHandlers(
    tryCatch(
      do.call(pred.fun, args),
      error = \(e) {
        err <<- conditionMessage(e)
        sapply(
          seq_len(NROW(newdata)),
          fallback
        )
      }
    ),
    warning = function(w) {
      # catches also warnings that are emitted by the above sapply call
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (!is.null(err)) { # TODO: also log warnings when do.call(pred.fun, args) does not produce an error
    logger::log_warn(
      sprintf(
        "%s: NAs inserted during $predict method call\n \b %s",
        model.info, paste0(unique(c(err, warn)), collapse = " \n \b ")
      )
    )
  }

  return(preds)
}
