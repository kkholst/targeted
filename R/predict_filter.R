predict_filter_bound <- function(lower = NULL, upper = NULL) {
  if (!is.null(lower) && !is.null(upper)) {
    if (lower > upper) stop(
      "lower bound should not be greater than upper bound."
    )
  }
  function(data) {
    function(pred, newdata) {
      if (!is.null(upper)) pred[pred > upper] <- upper
      if (!is.null(lower)) pred[pred < lower] <- lower
      pred
    }
  }
}

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
    function(pred, newdata) {
      if (lower) pred[pred < lb] <- lb
      if (upper) pred[pred > ub] <- ub
      pred
    }
  }
}
