model.extract2 <- function(frame, component) {
  # model.extract version that works with response,offset components
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

# extract variables of special terms. Returns results in a named list
Specials <- function(formula, spec, split1 = ",", split2 = NULL, ...) {
  tt <- terms(formula, spec)
  pos <- attributes(tt)$specials[[spec]]
  if (is.null(pos)) return(NULL)
  x <- rownames(attributes(tt)$factors)[pos]
  st <- gsub(" ", "", x) ## trim
  spec <- unlist(strsplit(st, "[()]"))[[1]]
  res <- substr(st, nchar(spec) + 2, nchar(st) - 1)
  if (!is.null(split1)) {
    res <- unlist(strsplit(res, split1))
  }
  res <- as.list(res)
  for (i in seq_along(res)) {
    if (length(grep("~", res[[i]])) > 0) {
      res[[i]] <- as.formula(res[[i]])
    }
  }
  return(res)
}

# labels of special terms (e.g. "weights(w)") whose variables are not all
# present in `data`
specials_unavailable <- function(terms, data) {
  pos <- unlist(attr(terms, "specials"))
  if (length(pos) < 1) return(character(0))
  labels <- rownames(attr(terms, "factors"))[pos]
  unavailable <- vapply(labels, function(lab) {
    vars <- all.vars(str2lang(lab))
    # skip pathological cases (e.g. `.` inside a special) to avoid false drops
    length(vars) > 0 && !("." %in% vars) && !all(vars %in% names(data))
  }, logical(1))
  return(labels[unavailable])
}

# remove terms (given by their labels) from a formula. The formula
# environment is preserved and the right-hand side collapses to `1` when no
# terms remain. The formula is reconstructed from its terms object, since
# update(formula, . ~ . - label) cannot remove offset terms, which are
# carried separately from the regular terms. `data` is only required for
# formulas containing a `.` on the right-hand side
remove_terms <- function(formula, labels, data = NULL) {
  tt <- terms(formula, data = data)
  vars <- attr(tt, "variables")
  offset.labels <- vapply(
    attr(tt, "offset"), function(i) deparse1(vars[[i + 1]]), character(1)
  )
  keep <- setdiff(c(attr(tt, "term.labels"), offset.labels), labels)
  if (length(keep) < 1) keep <- "1"
  response <- if (attr(tt, "response") == 1) vars[[2]] else NULL
  f <- reformulate(keep,
    response = response,
    intercept = attr(tt, "intercept") == 1
  )
  environment(f) <- environment(formula)
  return(f)
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
#' @param levels a named list of character vectors giving the full set of levels
#'   to be assumed for each factor
#' @param design.matrix (logical) if FALSE then only response and specials are
#'   returned. Otherwise, the design.matrix `x` is als part of the returned
#'   object.
#' @param na.action (function) method to handle missing data
#'   (default: \code{na.omit})
#' @return An object of class 'design'
#' @author Klaus Kähler Holst
#' @export
design <- function(formula, data, ..., # nolint
                   intercept = FALSE,
                   response = TRUE,
                   rm_envir = FALSE,
                   specials = NULL,
                   specials.call = NULL,
                   levels = NULL,
                   design.matrix = TRUE,
                   na.action = na.omit) {
  if (inherits(data, c("data.table", "tbl_df"))) {
    data <- as.data.frame(data)
  }
  dots <- substitute(list(...))
  if ("subset" %in% names(dots)) stop(
    "subset is not an allowed specials argument for targeted::design"
  )
  formulaenv <- environment(formula)

  # Remember the user's un-stripped formula so future update.design() calls can
  # re-extract specials if they become available again in new data.
  # stats::formula() is the identity for formulas and converts a terms object
  # into its formula (environment preserved in both cases).
  formula.original <- stats::formula(formula)

  tt <- terms(formula, data = data, specials = specials)

  # Drop specials whose variables are not available in `data`. This lets a
  # design built with e.g. `y ~ x + weights(w)` be (re-)evaluated against data
  # that lacks `w`: the special is silently omitted (its slot becomes NULL)
  # instead of failing in the subsequent model.frame() call.
  unavailable <- specials_unavailable(tt, data)
  if (length(unavailable) > 0) {
    formula <- remove_terms(formula, unavailable, data = data)
    tt <- terms(formula, data = data, specials = specials)
  }

  if (response && inherits(
    try(model.frame(update(tt, ~1), data = data, na.action = na.action),
        silent = TRUE),
    "try-error"
  )) { # response appears not to be in `data`
    response <- FALSE
  }
  # delete response to generate design matrix when making predictions
  if (!response) {
    tt <- delete.response(tt)
  }

  term.labels <- attr(tt, "term.labels") # predictors
  sterm.list <- c()
  if (length(specials) > 0) {
    des <- attr(tt, "factors")
    for (s in specials) {
      sterm <- rownames(des)[attr(tt, "specials")[[s]]]
      sterm.list <- c(sterm.list, sterm)
    }
    # predictors without the specials
    term.labels <- setdiff(term.labels, unlist(sterm.list))
    # remove special terms from formula
    formula <- remove_terms(formula, sterm.list, data = data)
  }

  formula0 <- formula
  environment(formula0) <- formulaenv # preserve formula environment
  if (!response) {
    formula0 <- stats::formula(delete.response(terms(formula, data = data)))
  }

  xlev <- levels
  xlev[["response_"]] <- NULL
  if (!design.matrix) { # only extract specials, response
    des <- attr(tt, "factors")
    fs <- update(formula0, ~1)
    if (length(sterm.list) > 0) {
      # formula with only special-terms
      fs <- reformulate(paste(sterm.list, collapse = " + "))
      fs <- update(formula0, fs)
    }
    environment(formula) <- formulaenv
    mf <- model.frame(fs, data = data, na.action = na.action, ...)
  } else { # also extract design matrix
    mf <- model.frame(tt,
                      data = data, ...,
                      xlev = xlev,
                      na.action = na.action,
                      drop.unused.levels = FALSE
                      )
    if (is.null(xlev)) {
      xlev <- .getXlevels(tt, mf)
    }
    xlev0 <- xlev
  }

  y <- NULL
  if (response) {
    y <- tryCatch(
      model.response(mf, type = "any"),
      error = function(...) NULL
    )
    if (is.factor(y) || is.character(y)) {
      ylev <- levels[["response_"]]
      if (!is.null(ylev)) {
        y <- factor(y, levels = ylev)
      } else {
        if (is.factor(y)) {
          ylev <- levels(y)
        }
        if (is.null(levels[["response_"]])) {
          levels[["response_"]] <- ylev
        }
      }
    }
  }

  has_intercept <- attr(tt, "intercept") == 1L
  specials <- union(
    specials,
    names(dots)[-1] # removing "" at first position when calling dots, which
  ) # is a call object

  specials.list <- c()
  specials.var <- c() # holds the variable-arguments of the specials functions
  if (length(specials) > 0) {
    for (s in specials) {
      w <- eval(substitute(model.extract2(mf, s), list(s = s)))
      specials.list <- c(specials.list, list(w))
      specials.var <- c(
        specials.var,
        list(unlist(Specials(tt, spec = s)))
      )
    }
    names(specials.var) <- specials
    names(specials.list) <- specials
    if (length(sterm.list) > 0) {
      if (design.matrix) {
        xlev0[sterm.list] <- NULL
        mf <- model.frame(formula0,
                          data = data, ...,
                          xlev = xlev0,
                          na.action = na.action,
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
    x <- model.matrix(mf, data=mf)
    if (!intercept && has_intercept) {
      has_intercept <- FALSE
      x <- x[, -1, drop = FALSE]
    }
  } else {
    term.labels <- NULL
    x <- NULL
  }

  if (rm_envir) attr(tt, ".Environment") <- NULL
  if (is.null(specials.call)) specials.call <- dots

  xlev[["response_"]] <- levels[["response_"]]
  res <- c(
    list(
      formula = formula, # formula without specials
      formula.original = formula.original, # user-provided formula (internal)
      terms = tt,
      term.labels = term.labels,
      levels = xlev,
      x = x, y = y,
      design.matrix = design.matrix,
      intercept = has_intercept,
      data = data[0, ], ## Empty data.frame to capture structure of data
      specials = specials,
      na.action = na.action,
      specials.var = specials.var,
      specials.call = specials.call
    ),
    specials.list
  )
  return(structure(res, class="design"))
}

#' @export
update.design <- function(object, data = NULL, response = FALSE, levels, ...) {
  if (is.null(data)) data <- object$data
  if (missing(levels)) levels <- object$levels
  return(
    design(object$formula.original,
      data = data,
      design.matrix = object$design.matrix,
      levels = levels,
      intercept = object$intercept,
      specials = object$specials,
      specials.call = object$specials.call,
      na.action = object$na.action,
      response = response
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
    y <- x$y
    ## colnames(y) <- ""
    if (is.factor(y)) y <- as.character(y)
    if (inherits(y, c("Surv", "Event"))) {
      cat("\n")
      y <- cbind(y)
    }
    cat("\n")
    lava::Print(y, n = n, ...)
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
