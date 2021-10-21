##' Similar to `expand.grid` function, this function creates all combinations
##' of the input arguments but returns the result as a list.
##' @title Create a list from all combination of input variables
##' @param ... input variables
##' @return list
##' @author Klaus Kähler Holst
##' @examples
##' expand.list(x=2:4, z=c("a","b"))
##' @export
expand.list <- function(...) {
  dots <- list(...)
  nam <- names(dots)
  nulls <- c()
  formulas <- c()
  for (i in seq_along(dots)) {
    if (is.null(dots[[i]])) {
      dots[[i]] <- "NULL"
      nulls <- c(nulls, i)
    }
    if (inherits(dots[[i]], "formula")) {
      dots[[i]] <- deparse1(dots[[i]])
      formulas <- c(formulas, i)
    }
  }
  names(dots) <- nam
  dat <- do.call(expand.grid, c(dots,
            list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
  res <- lapply(seq(NROW(dat)),
                function(i) {
                  res <- as.list(dat[i, ])
                  if (length(nulls)>0) res[nulls] <- list(NULL)
                  if (length(formulas)>0) {
                    res[[formulas]] <- as.formula(res[[formulas]])
                    environment(res[[formulas]]) <- emptyenv()
                  }
                  res
                })
  structure(res, table=dat)
}
