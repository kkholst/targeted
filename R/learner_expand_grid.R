#' Construct learners from a grid of parameters
#' @param fun (function) A function that returns a [learner].
#' @param args (list) Parameters that generate a grid of parameters with
#' [expand.list], where the set of parameters are then passed on to `fun`.
#' @param names (logical or character) If FALSE, then return a list without
#' names. If TRUE, a named list is returned (see details).
#' @param params (logical) If FALSE, then no information about the parameters
#' defined by `args` are added to the names of the returned list.
#' @return list
#' @export
#' @examples
#' lrs <- learner_expand_grid(
#'   learner_xgboost,
#'   list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3))
#' )
#' lrs # use info of constructed learner as names
#'
#' lrs <- learner_expand_grid(
#'   learner_xgboost,
#'   list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
#'   names = "xgboost"
#' )
#' names(lrs) # use xgboost instead of info field for names
#'
#' lrs <- learner_expand_grid(
#'   learner_xgboost,
#'   list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
#'   names = "xgboost",
#'   params = TRUE
#' )
#' names(lrs) # also add parameters to names
#'
#' lrs <- learner_expand_grid(
#'   learner_xgboost,
#'   list(formula = Sepal.Length ~ ., eta = c(0.2, 0.5, 0.3)),
#'   names = FALSE
#' )
#' names(lrs) # unnamed list since names = FALSE
learner_expand_grid <- function(fun, args, names = TRUE, params = FALSE) {
  args <- do.call(expand.list, args)
  lrs <- lapply(args, function(par) do.call(fun, par))

  if (!is.character(names) && names) {
    .name <- lrs[[1]]$info
  } else {
    .name <- NULL
  }
  if (is.character(names)) .name <- names

  if (params) {
    .names <- c()
    for (i in seq_along(lrs)) {
      param <- paste0(attr(args, "table")[i, ], collapse = ":")
      .names <- c(.names, paste0(.name, ":", param))
    }
  } else {
    .names <- rep(.name, length(lrs))
  }
  if (!is.null(.name)) {
    names(lrs) <- .names
    names(lrs) <- make.unique(names(lrs))
  }

  return(lrs)
}
