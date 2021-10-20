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
  for (i in seq_along(dots)) {
    if (is.null(dots[[i]])) {
      dots[[i]] <- NA
      nulls <- c(nulls, i)
    }
  }
  names(dots) <- nam
  dat <- do.call(expand.grid, c(dots, list(KEEP.OUT.ATTRS = FALSE)))
  lapply(seq(NROW(dat)),
         function(i) {
           res <- as.list(dat[i, ])
           if (length(nulls)>0) res[nulls] <- list(NULL)
           res
           })
}
