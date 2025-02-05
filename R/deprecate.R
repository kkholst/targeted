# to be used as default value of deprecated arguments to inform users in
# roxygen documentation that argument is deprecated
deprecated <- function() "missing argument"

#' Cast warning for deprecated function argument names
#' @param old deprecated argument name
#' @param new argument that should be used instead
#' @param fun function name where arguments are deprecated
#' @param vers version when argument is deprecated
deprecate_arg_warn <- function(old, new, fun, vers) {
  warning(sprintf(
    "The `%s` argument of `%s()` is deprecated as of targeted %s. ",
    old, fun, vers
  ), sprintf("Please use the `%s` argument instead.", new),
  call. = FALSE
  )
}

#' Deprecated argument names
#' @name deprecated_argument_names
#' @param response_model Deprecated. Use response.model instead.
#' @param propensity_model Deprecated. Use propensity.model instead.
#' @param cate_model Deprecated. Use cate.model instead.
#' @param g_model Deprecated. Use g.model instead.
#' @param treatment Deprecated. Use cate.model instead.
# list of deprecated argument names. can be used in roxygen documentation
# via @inheritParams deprecated_argument_names
NULL
