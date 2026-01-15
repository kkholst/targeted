#' Similar to `expand.grid` function, this function creates all combinations
#' of the input arguments but returns the result as a list.
#' @title Create a list from all combination of input variables
#' @param ... input variables
#' @param INPUT optional list of variables
#' @param envir environment environment to evalute formulas in
#' @return list
#' @author Klaus Kähler Holst
#' @examples
#' expand.list(x = 2:4, z = c("a", "b"))
#' @export
expand.list <- function(..., INPUT = NULL, envir = NULL) {
  inp <- c(INPUT, list(...))
  for (i in seq_along(inp)) {
    if (is.null(inp[[i]])) {
      inp[[i]] <- list(NULL)
    }
    if (!is.vector(inp[[i]]) &&
        !inherits(inp[[i]], "list")
        ) {
      inp[[i]] <- list(inp[[i]])
    }
  }
  nn <- lapply(lapply(inp, length), seq_len)
  gr <- expand.grid(nn)
  res <- c()
  for (i in seq_len(nrow(gr))) {
    idx <- unlist(gr[i, ])
    r <- c()
    for (j in seq_along(idx)) {
      r <- c(r, list(inp[[j]][[idx[j]]]))
    }
    names(r) <- names(inp)
    res <- c(res, list(r))
  }
  return(res)
}
