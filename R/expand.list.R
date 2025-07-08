#' Similar to `expand.grid` function, this function creates all combinations
#' of the input arguments but returns the result as a list.
#' @title Create a list from all combination of input variables
#' @param ... input variables
#' @param INPUT optional list of variables
#' @param envir environment environment to evalute formulas in
#' @return list
#' @author Klaus Kähler Holst
#' @examples
#' expand.list(x=2:4, z=c("a","b"))
#' @export
expand.list <- function(..., INPUT = NULL, envir = NULL) {
  dots <- c(INPUT, list(...))
  nam <- names(dots)
  nulls <- c()
  formulas <- c()
  for (i in seq_along(dots)) {
    if (is.null(dots[[i]])) {
      dots[[i]] <- "NULL"
      nulls <- c(nulls, i)
    }
    if (is.list(dots[[i]])) {
      if (inherits(dots[[i]][[1]], "formula")) {
        if (is.null(envir)) envir <- environment(dots[[i]][[1]])
        dots[[i]] <- unlist(lapply(dots[[i]], deparse1))
        formulas <- c(formulas, i)
      }
    } else if (inherits(dots[[i]], "formula")) {
      if (is.null(envir)) envir <- environment(dots[[i]])
      dots[[i]] <- deparse1(dots[[i]])
      formulas <- c(formulas, i)
    }
  }

  names(dots) <- nam
  dat <- do.call(
    expand.grid,
    c(dots, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  )

  fun <- function(i) {
    res <- as.list(dat[i, ])
    if (length(nulls)>0) res[nulls] <- list(NULL)
    if (length(formulas)>0) {
      res[[formulas]] <- as.formula(res[[formulas]])
      environment(res[[formulas]]) <- envir
    }
    return(res)
  }
  res <- lapply(seq_len(NROW(dat)), fun)

  return(structure(res, table=dat))
}
