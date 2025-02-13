model.extract2 <- function(frame, component) {
  component <- as.character(substitute(component))
  if (component %in% c("response", "offset")) {
    return(do.call(
      model.extract,
      list(frame = frame, component = component)
    ))
  }
  vname <- paste0("(", component, ")")
  if (!(vname %in% names(frame))) {
    regex <- paste0("^", component, "\\(.*\\)$")
    if (any(grepl(regex, names(frame)))) {
      vname <- grep(regex, names(frame))
      if (length(vname) > 1) stop("model.extract2: non-unique component")
    }
  }
  rval <- frame[[vname]]

  if (!is.null(rval)) {
    if (length(rval) == nrow(frame)) {
      names(rval) <- attr(frame, "row.names")
    } else if (is.matrix(rval) && nrow(rval) == nrow(frame)) {
      t1 <- dimnames(rval)
      dimnames(rval) <- list(
        attr(frame, "row.names"),
        t1[[2L]]
      )
    }
  }
  rval
}

#' Extract design matrix from data.frame and formula
#' @title Extract design matrix
#' @param formula formula
#' @param data data.frame
#' @param intercept (logical) If FALSE an intercept is not included in the
#'   design matrix
#' @param response (logical) if FALSE the response variable is dropped
#' @param rm_envir Remove environment
#' @param ... additional arguments (e.g, specials such weights, offsets, ...)
#' @param specials character vector specifying functions in the formula that
#'   should be marked as special in the [terms] object
#' @param specials.call (call) specials optionally defined as a call-type
#' @param xlev a named list of character vectors giving the full set of levels
#'   to be assumed for each factor
#' @return An object of class 'design'
#' @author Klaus Kähler Holst
#' @export
design <- function(formula, data, ...,
                   intercept = FALSE,
                   response = FALSE,
                   rm_envir = FALSE,
                   specials = c("weights", "offset"),
                   specials.call = NULL,
                   xlev = NULL) {
  tt <- terms(formula, data = data, specials = specials)
  dots <- substitute(list(...))
  if ("subset" %in% names(dots)) stop(
    "subset is not an allowed specials argument for targeted::design"
  )
  mf <- model.frame(tt,
    data = data, ...,
    xlev = xlev,
    drop.unused.levels = FALSE
    )
  mf <- model.frame(tt, data=data, ...)

  y <- model.response(mf, type = "any")
  # delete response to generate design matrix when creating making predictions
  if (!response) tt <- delete.response(tt)
  specials <- union(
    specials,
    names(dots)[-1] # removing "" at first position when calling dots, which
  ) # is a call object
  if (is.null(xlev)) {
    xlev <- .getXlevels(tt, mf)
  }
  xlev0 <- xlev
  specials.list <- c()
  if (length(specials) > 0) {
    des <- attr(tt, "factors")

    sterm.list <- c()
    for (s in specials) {
      w <- eval(substitute(model.extract2(mf, s), list(s = s)))
      specials.list <- c(specials.list, list(w))
      sterm <- rownames(des)[attr(tt, "specials")[[s]]]
      sterm.list <- c(sterm.list, sterm)
    }
    names(specials.list) <- specials
    if (length(sterm.list) > 0) {
      upd <- paste(" ~ . - ", paste(sterm.list, collapse = " - "))
      tmp.terms <- update(tt, upd) |> terms()
      xlev0 <- .getXlevels(tmp.terms, mf)
      mf <- model.frame(tmp.terms,
        data = data, ...,
        xlev = xlev0,
        drop.unused.levels = FALSE
      )
    }
  }
  if (!is.null(specials.call)) {
    specials.list2 <- eval(specials.call, data)
    for (n in names(specials.list2)) {
      if (is.null(specials.list[[n]])) {
        specials.list[[n]] <- specials.list2[[n]]
      }
    }
  }

  x <- model.matrix(mf, data = data, xlev = xlev0)
  has_intercept <- attr(tt, "intercept") == 1L
  if (!intercept && has_intercept) {
    has_intercept <- FALSE
    x <- x[, -1, drop = FALSE]
  }

  if (rm_envir) attr(tt, ".Environment") <- NULL
  if (is.null(specials.call)) specials.call <- dots

  res <- c(
    list(
      terms = tt, xlevels = xlev, x = x, y = y,
      intercept = has_intercept,
      data = data[0, ], ## Empty data.frame to capture structure of data
      specials = specials,
      specials.call = specials.call
    ),
    specials.list
  )
  structure(res, class="design")
}

#' @export
update.design <- function(object, data = NULL, ...) {
  if (is.null(data)) data <- object$data
  design(object$terms,
    data = data,
    xlev = object$xlevels,
    specials = object$specials,
    specials.call = object$specials.call
  )
}

#' @export
model.matrix.design <- function(object, ...) {
  return(object$x)
}

#' @export
weights.design <- function(object, ...) {
  specials(object, "weights")
}

#' @export
offsets <- function(object, ...) UseMethod("offsets")

#' @export
offsets.design <- function(object, ...) {
  specials(object, "offset")
}

#' @export
specials <- function(object, ...) UseMethod("specials")

#' @export
#' @title Extract model component from [design] object
#' @param object [design] object
#' @param which model component (e.g., "offset", "weights", ...)
#' @aliases specials specials.design offsets offsets.design weights.design
#' @param ...  Additional arguments to lower level functions
specials.design <- function(object, which, ...) {
  return(object[[which]])
}

#' @export
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
