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
  return(rval)
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
#' @param design.matrix (logical) if FALSE then only response and specials are
#'   returned. Otherwise, the design.matrix `x` is als part of the returned
#'   object.
#' @return An object of class 'design'
#' @author Klaus Kähler Holst
#' @export
design <- function(formula, data, ..., # nolint
                   intercept = FALSE,
                   response = FALSE,
                   rm_envir = FALSE,
                   specials = NULL,
                   specials.call = NULL,
                   xlev = NULL,
                   design.matrix = TRUE) {
  dots <- substitute(list(...))
  if ("subset" %in% names(dots)) stop(
    "subset is not an allowed specials argument for targeted::design"
  )
  tt <- terms(formula, data = data, specials = specials)
  term.labels <- attr(tt, "term.labels") # predictors

  sterm.list <- c()
  if (length(specials) > 0) {
    des <- attr(tt, "factors")
    for (s in specials) {
      sterm <- rownames(des)[attr(tt, "specials")[[s]]]
      sterm.list <- c(sterm.list, sterm)
    }
    if (length(sterm.list) > 0) {
      # create formula without specials
      if ((nrow(attr(tt, "factors")) - attr(tt, "response")) ==
          length(sterm.list)) {
        # only specials on the rhs, remove everything
        formula <- update(formula, ~1)
      } else {
        # remove specials from formula
        ## sterm.idx <- unlist(attr(tt, "specials")) - attr(tt, "response")
        # predictors without the specials
        term.labels <- setdiff(term.labels,
                               unlist(sterm.list))
        ## xx <- attr(tt, "term.labels")[-sterm.idx]
        ## formula <- update(tt, reformulate(xx))
        formula <- update(tt, reformulate(term.labels))
      }

      upd <- paste(" ~ . - ", paste(sterm.list, collapse = " - "))
      formula <- update(formula, upd)
    }
  }

  if (!design.matrix) { # only extract specials, response
    des <- attr(tt, "factors")
    fs <- update(formula, ~1)
    if (length(sterm.list) > 0) {
      # formula with only special-terms
      fs <- reformulate(paste(sterm.list, collapse = " + "))
      fs <- update(formula, fs)
    }
    mf <- model.frame(fs, data=data, ...)
  } else { # also extract design matrix
    mf <- model.frame(tt,
                      data = data, ...,
                      xlev = xlev,
                      drop.unused.levels = FALSE
                      )
    if (is.null(xlev)) {
      xlev <- .getXlevels(tt, mf)
    }
    xlev0 <- xlev
  }

  y <- model.response(mf, type = "any")
  has_intercept <- attr(tt, "intercept") == 1L
  specials <- union(
    specials,
    names(dots)[-1] # removing "" at first position when calling dots, which
  ) # is a call object

  specials.list <- c()
  if (length(specials) > 0) {
    for (s in specials) {
      w <- eval(substitute(model.extract2(mf, s), list(s = s)))
      specials.list <- c(specials.list, list(w))
    }
    names(specials.list) <- specials
    if (length(sterm.list) > 0) {
      if (design.matrix) {
        xlev0[sterm.list] <- NULL
        mf <- model.frame(formula,
                          data = data, ...,
                          xlev = xlev0,
                          drop.unused.levels = FALSE
                          )
      }
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

  if (design.matrix) {
    x <- model.matrix(mf, data = data, xlev = xlev0)
    if (!intercept && has_intercept) {
      has_intercept <- FALSE
      x <- x[, -1, drop = FALSE]
    }
  } else {
    term.labels <- NULL
    x <- NULL
  }

  # delete response to generate design matrix when making predictions
  if (!response) tt <- delete.response(tt)

  if (rm_envir) attr(tt, ".Environment") <- NULL
  if (is.null(specials.call)) specials.call <- dots

  res <- c(
    list(
      formula = formula, # formula without specials
      terms = tt,
      term.labels = term.labels,
      xlevels = xlev,
      x = x, y = y,
      design.matrix = design.matrix,
      intercept = has_intercept,
      data = data[0, ], ## Empty data.frame to capture structure of data
      specials = specials,
      specials.call = specials.call
    ),
    specials.list
  )
  return(structure(res, class="design"))
}

#' @export
update.design <- function(object, data = NULL, ...) {
  if (is.null(data)) data <- object$data
  return(
    design(object$terms,
      data = data,
      design.matrix = object$design.matrix,
      xlev = object$xlevels,
      intercept = object$intercept,
      specials = object$specials,
      specials.call = object$specials.call
    )
  )
}

#' @export
model.matrix.design <- function(object, ...) {
  return(object$x)
}

#' @export
#' @title Extract model component from [design] object
#' @param x [design] object
#' @param specials extract variables marked as special
#' (e.g., "offset", "weights", ...)
#' @param ...  Additional arguments to lower level functions
terms.design <- function(x, specials, ...) {
  if (missing(specials)) return(x$terms)
  return(x[[specials]])
}

#' @export
summary.design <- function(object, ...) {
  object$x <- object$x[0, , drop=FALSE]
  object$y <- NULL
  for (i in object$specials) object[[i]] <- NULL
  return(object)
}

#' @export
print.design <- function(x, n=2, ...) {
  cat_ruler(" design object ", 10)
  cat(sprintf("\nresponse (length: %s)", length(x$y)))
  if (length(x$y) > 0) {
    lava::Print(x$y, n = n, ...)
  } else {
    cat("\n")
  }
  specials <- c()
  for (nam in x$specials) {
    if (!is.null(x[[nam]])) {
      specials <- c(specials, nam)
    }
  }
  if (length(specials) > 0) {
    cat("\nspecials")
    for (nam in specials) {
        cat(paste0("\n - ", nam, " [", class(x[[nam]]), "]"))
    }
    cat("\n")
  }
  cat(sprintf("\ndesign matrix (dim: %s)\n", paste0(dim(x$x), collapse = ", ")))
  if (NROW(x$x) > 0) {
    lava::Print(x$x, n = n, ...)
  } else {
    print(x$x)
  }
  return(invisible(x))
}

get_response <- function(formula, ...) {
  if (!is.null(attr(formula, "response"))) {
    y <- get(attr(formula, "response"), envir=environment(formula))
  } else {
    y <- model.response(model.frame(formula, ...))
  }
  return(y)
}
