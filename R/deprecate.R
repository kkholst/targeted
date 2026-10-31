# to be used as default value of deprecated arguments to inform users in
# roxygen documentation that argument is deprecated
deprecated <- function() "missing argument"

#' Cast warning for deprecated function argument names
#' @param old deprecated argument name
#' @param new argument that should be used instead
#' @param fun function name where arguments are deprecated
#' @param vers version when argument is deprecated
deprecate_arg_warn <- function(old, new, fun, vers) {
  warning(
    sprintf(
      paste0(
        "The `%s` argument of `%s()` is deprecated ",
        "and will be removed in targeted %s. "
      ),
      old, fun, vers
    ),
    sprintf("Please use the `%s` argument instead.", new),
    call. = FALSE
  )
}
