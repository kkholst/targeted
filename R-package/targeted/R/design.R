##' Extract design matrix from data.frame and formula
##' @title Extract design matrix
##' @param formula formula
##' @param data data.frame
##' @param intercept If FALSE (default) an intercept is not included
##' @param rm_envir Remove environment
##' @param ... additional arguments (e.g, specials such weights, offsets, subset)
##' @return An object of class 'design'
##' @author Klaus Kähler Holst
##' @export
design <- function(formula, data, intercept=FALSE,
                   rm_envir=FALSE, ...) {
  tt <- terms(formula, data=data)
  if (!intercept)
    attr(tt, "intercept") <- 0
  mf <- model.frame(tt, data=data, ...)
  x_levels <- .getXlevels(tt, mf)
  x <- model.matrix(mf, data=data)
  y <- model.response(mf, type="any")
  specials <- names(substitute(list(...)))[-1]
  specials_list <- c()
  if (length(specials)>0) {
    for (s in specials) {
      w <- eval(substitute(model.extract(mf, s), list(s=s)))
      specials_list <- c(specials_list, list(w))
    }
    names(specials_list) <- specials
  }
  tt <- delete.response(tt)
  if (rm_envir)
    attr(tt, ".Environment") <- NULL
  res <- c(list(terms=tt, xlevels=x_levels, x=x, y=y,
                data=data[0, ], ## Empty data.frame to capture structure of data
                specials=specials),
           specials_list)
  structure(res, class="design")
}

##' @export
update.design <- function(object, data=NULL, ...) {
  if (is.null(data))  data <- object$data
  mf <- with(object, model.frame(terms, data=data, ...,
                                 xlev = xlevels,
                                 drop.unused.levels=FALSE))
  x <- model.matrix(mf, data=data, ..., xlev = object$xlevels)
  object[["y"]] <- NULL
  for (s in object$specials) {
    object[[s]] <- NULL
  }
  specials <- names(substitute(list(...)))[-1]
  for (s in specials) {
    object[[s]] <- eval(substitute(model.extract(mf, s), list(s=s)))
  }
  object$specials <- specials
  object$x <- x
  return(object)
}

##' @export
model.matrix.design <- function(object, drop.intercept = FALSE, ...) {
  if (drop.intercept) {
    intercept <- which(attr(object$x, "assign") == 0)
    if (length(intercept)>0)
      return(object$x[, -intercept, drop=FALSE])
  }
  return(object$x)
}

##' @export
summary.design <- function(object, ...) {
  object$x <- object$x[0, ]
  object$y <- NULL
  for (i in object$specials) object[[i]] <- NULL
  return(object)
}

get_response <- function(formula, ...) {
  if (!is.null(attr(formula, "response"))) {
    y <- get(attr(formula, "response"), envir=environment(formula))
  } else {
    y <- model.response(model.frame(formula, ...))
  }
  return(y)
}
