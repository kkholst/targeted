Identical <- function(x, y = 1, tolerance = .Machine$double.eps^0.5) {
  return(Mod(x - y) < tolerance)
}

rd2pr <- function(rd, op) {
  a <- op - 1
  b <- -op * (2 - rd) - rd
  p0 <- (-b - sqrt(b^2 - 4 * op * (1 - rd) * a)) / (2 * a)
  op1 <- lapply(op, function(x) Identical(x, 1)) |>
    unlist() |>
    which()
  if (length(op1) > 0) {
    p0[op1] <- 0.5 * (1 - rd[op1])
  }
  p1 <- p0 + rd
  return(cbind(p0, p1))
}

rr2pr <- function(rr, op) {
  b <- op * (1 + rr)
  a <- rr * (1 - op)
  p0 <- (-b + sqrt(b^2 + 4 * a * op)) / (2 * a)
  op1 <- lapply(op, function(x) Identical(x, 1)) |>
    unlist() |>
    which()
  if (length(op1) > 0) {
    p0[op1] <- 1 / (1 + rr[op1])
  }
  p1 <- p0 * rr
  return(cbind(p0, p1))
}

add_offset <- function(formula, offset) {
  tryCatch(offset, error = function(e) {
      stop("Non-standard evaluations are not supported by the offset argument.")
  })

  if (!is.null(offset)) {
    newf <- paste0(".~.+offset(", offset, ")")
    formula <- update(formula, as.formula(newf))
  }
  return(formula)
}

# Add optional arguments to function
add_dots <- function(f) {
  if (!is.function(f)) {
    stop("f is not a function.")
  }
  if (!("..." %in% formalArgs(f))) {
    formals(f) <- c(formals(f), alist(... = ))
  }
  return(f)
}

list2str <- function(x) {
  nn <- names(x)
  val <- paste(x)
  n <- length(val)
  res <- ""
  for (i in seq_along(val)) {
    res <- paste0(res, nn[i], "=", val[i])
    if (i<n) res <- paste0(res, ", ")
  }
  return(res)
}
