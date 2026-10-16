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
#' @param rm.envir (logical) Remove environment from terms attribute of returned
#'   object
#' @param ... additional named arguments (e.g., specials such as `weights = w`
#'   or `offset = o`) that are passed on to [stats::model.frame] and returned as
#'   specials. Unnamed arguments and `subset` are not allowed.
#' @param specials character vector specifying functions in the formula that
#'   should be marked as special in the [terms] object. Functions are looked up
#'   by following the lexical scoping of `model.frame` (see details). Specials
#'   that are missing in the formula will create special fields in the returned
#'   design object with their value set to `NULL`.
#' @param specials.call (call) specials optionally defined as a call-type
#' @param levels a named list of character vectors giving the full set of levels
#'   to be assumed for each factor
#' @param design.matrix (logical) if FALSE then only response and specials are
#'   returned. Otherwise, the design.matrix `x` is als part of the returned
#'   object.
#' @param na.action (function) method to handle missing data (default:
#'   \code{na.omit})
#' @details Special terms are identified syntactically by the name of the
#'   called function (see [stats::terms.formula]). For example, with
#'   `specials = "stratify"`, the term `stratify(a)` is a special whereas
#'   `targeted::stratify(a)` is not.
#'
#'   The special terms are evaluated by [stats::model.frame] in `data` with the
#'   environment of `formula` as the enclosing environment. Following the
#'   lexical scoping rules of R, a special function is thus looked up in the
#'   environment of `formula` (by default the environment in which the formula
#'   was created) and its parent environments (e.g., the global environment and
#'   attached packages), but not in the environment from which `design()` is
#'   called. Hence, a special function must be visible from the environment of
#'   `formula`. The environment of `formula` is kept in the returned object such
#'   that the same lookup applies when the design is re-evaluated with
#'   [update.design].
#'
#'   Note that a special term is dropped (i.e., its field in the returned
#'   object is `NULL`) if any variable referenced in the term is not a column
#'   of `data`, even if the variable exists in the environment of `formula`.
#' @return An object of class 'design'
#' @author Klaus Kähler Holst
#' @seealso [update.design]
#' @examples
#' n <- 1e2
#' d0 <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
#' d0$w <- runif(n)
#'
#' dd <- design(y ~ a + x, data = d0)
#' list(response = head(dd$y, 2), design.matrix = head(dd$x, 2))
#'
#' # specials
#' # special variable is extracted and removed from design matrix
#' dd <- design(y ~ a + x + weights(w), d0, specials = "weights")
#' list(weights = head(dd$weights, 2), design.matrix = head(dd$x, 2))
#' # declared special variable is missing in formula. the associated field
#' # is NULL
#' dd <- design(y ~ a + x, d0, specials = "weights")
#' list(weights = dd$weights, design.matrix = head(dd$x, 2))
#' @export
design <- function(formula, data, ..., # nolint
                   intercept = FALSE,
                   response = TRUE,
                   rm.envir = FALSE,
                   specials = NULL,
                   specials.call = NULL,
                   levels = NULL,
                   design.matrix = TRUE,
                   na.action = na.omit) {
  # Soft-deprecation: `rm_envir` was renamed to `rm.envir`. When supplied it is
  # now captured by `...`; intercept it, warn, and re-dispatch with the new
  # name so it is not mistaken for a special.
  mc <- match.call()
  if ("rm_envir" %in% names(mc)) {
    deprecate_arg_warn("rm_envir", "rm.envir", "design", "0.9.5")
    mc[["rm.envir"]] <- mc[["rm_envir"]]
    mc[["rm_envir"]] <- NULL
    return(eval(mc, parent.frame()))
  }
  if (inherits(data, c("data.table", "tbl_df"))) {
    data <- as.data.frame(data)
  }

  if (!is.character(specials) && !is.null(specials)) {
    stop("The special argument must be a character vector.")
  }

  dots <- substitute(list(...))
  dots.names <- names(dots)[-1L]
  if (is.null(dots.names)) dots.names <- character(length(dots) - 1L)
  # `...` is forwarded to model.frame() right after `data`. Unnamed arguments
  # would be matched by position (and abbreviated names such as `sub` by
  # partial matching) to its `subset` argument (#241)
  unnamed <- !nzchar(dots.names)
  if (any(unnamed)) {
    exprs <- vapply(as.list(dots)[-1L][unnamed],
      \(e) deparse(e, nlines = 1L)[1L], character(1)
    )
    stop(
      "all arguments in `...` must be named (e.g., `weights = w`). ",
      "Unnamed: ", paste(exprs, collapse = ", ")
    )
  }
  if (any(startsWith("subset", dots.names))) stop(
    "subset is not an allowed specials argument for targeted::design"
  )
  formulaenv <- environment(formula)

  # Remember the user's un-stripped formula so future update.design() calls can
  # re-extract specials if they become available again in new data.
  # stats::formula() is the identity for formulas and converts a terms object
  # into its formula (environment preserved in both cases).
  formula.original <- stats::formula(formula)

  tt <- terms(formula, data = data, specials = specials)

  n_specials <- sapply(attr(tt, "specials"), length)
  if (any(n_specials > 1)) {
    stop(
      "Duplicated specials defined in formula. Specify only a single",
      " definition in the formula for each special."
    )
  }

  # Drop specials whose variables are not available in `data`. This lets a
  # design built with e.g. `y ~ x + weights(w)` be (re-)evaluated against data
  # that lacks `w`: the special is silently omitted (its slot becomes NULL)
  # instead of failing in the subsequent model.frame() call.
  unavailable <- specials_unavailable(tt, data)
  if (length(unavailable) > 0) {
    formula <- remove_terms(formula, unavailable, data = data)
    tt <- terms(formula, data = data, specials = specials)
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

  if (response && inherits(
    try(model.frame(update(tt, ~1), data = data, na.action = na.action),
        silent = TRUE),
    "try-error"
  )) { # response appears not to be in `data`
    response <- FALSE
  }

  formula0 <- formula
  environment(formula0) <- formulaenv # preserve formula environment
  if (!response) {
    formula0 <- stats::formula(delete.response(terms(formula, data = data)))
    # delete response to generate design matrix when making predictions
    tt <- delete.response(tt)
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
                      data = data, ..., # specials provided via ...
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

  if (rm.envir) attr(tt, ".Environment") <- NULL
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

#' Update method for design
#' @export
#' @param object (design) object returned by [design]
#' @param data (data.frame) data on which the design is re-evaluated. If `NULL`,
#'   the empty data.frame stored in `object` is used.
#' @param response (logical) if FALSE the response variable is dropped
#' @param levels (list) named list of character vectors giving the full set of
#'   levels to be assumed for each factor. Defaults to the levels stored in
#'   `object`, such that factors are coded as in the original design.
#' @param ... additional arguments (currently ignored)
#' @return An object of class 'design'
#' @examples
#' n <- 1e2
#' d0 <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
#' d0$w <- runif(n)
#' dnew <- data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n), y = rnorm(n))
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
