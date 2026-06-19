#' @title Prediction filter bounding predictions to fixed range
#' @description Generates a prediction filter for the `predict.filter` argument
#'   of [learner] that bounds predictions to a fixed range (lower, upper).
#' @param lower (numeric) Lower bound, or `NULL` for no lower bound.
#' @param upper (numeric) Upper bound, or `NULL` for no upper bound.
#' @return A filter generator function (see [learner]).
#' @author Benedikt Sommer
#' @export
predict_filter_bound <- function(lower = NULL, upper = NULL) {
  if (!is.null(lower) && !is.null(upper)) {
    if (lower > upper) stop(
      "lower bound should not be greater than upper bound."
    )
  }
  function(data) {
    function(pred) {
      if (!is.null(upper)) pred[pred > upper] <- upper
      if (!is.null(lower)) pred[pred < lower] <- lower
      pred
    }
  }
}

#' @title Prediction filter bounding predictions to the observed response range
#' @description Generates a prediction filter for the `predict.filter`
#'   argument of [learner] that bounds predictions to the range of the
#'   response observed in the estimation data (min(response), max(response)).
#' @param lower (logical) If `TRUE`, clamp to the minimum observed response.
#' @param upper (logical) If `TRUE`, clamp to the maximum observed response.
#' @param response (character) Name of the response column in the data.
#' @return A filter generator function (see [learner]).
#' @author Benedikt Sommer
#' @export
predict_filter_bound_dynamic <- function(
    lower = FALSE,
    upper = FALSE,
    response = "y"
  ) {
  if (!is.logical(lower) || !is.logical(upper)) stop(
    "lower and upper argument need to be logical."
  )
  if (!is.character(response)) stop(
    "response argument needs to be a character."
  )

  function(data) {
    if (lower) lb <- min(data[[response]])
    if (upper) ub <- max(data[[response]])
    function(pred) {
      if (lower) pred[pred < lb] <- lb
      if (upper) pred[pred > ub] <- ub
      pred
    }
  }
}
