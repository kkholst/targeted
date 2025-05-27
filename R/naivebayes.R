#' Naive Bayes Classifier
#'
#' @title Naive Bayes classifier
#' @param formula Formula with syntax: response ~ predictors | weights
#' @param data data.frame
#' @param weights optional frequency weights
#' @param kernel If TRUE a kernel estimator is used for numeric predictors
#' (otherwise a gaussian model is used)
#' @param laplace.smooth Laplace smoothing
#' @param prior optional prior probabilities (default estimated from data)
#' @param ... additional arguments to lower level functions
#' @aliases naivebayes NB
#' @export
#' @return An object of class '\code{naivebayes}' is returned. See
#'   \code{\link{naivebayes-class}} for more details about this class and
#'   its generic functions.
#' @author Klaus K. Holst
#' @examples
#' data(iris)
#' m2 <- naivebayes(Species ~ Sepal.Width + Petal.Length, data = iris)
#' pr2 <- predict(m2, newdata = iris)
naivebayes <- NB <-
  function(formula, data, weights = NULL,
               kernel = FALSE, laplace.smooth = 0, prior = NULL, ...) {
  
  if (missing(data)) stop("Need data as data.frame or data.table")
  if (!data.table::is.data.table(data)) data <- data.table::data.table(data)
  des <- design(formula, data = as.data.frame(data), specials = "weights")
  y <- as.factor(des$y)
  if (is.null(weights)) {
    weights <- des$weights
    if (is.null(weights)) weights <- rep(1, length(y))
  }
  predictor <- des$term.labels
  X <- data[, predictor, with = FALSE, drop = FALSE]
  charvar <- names(Filter(is.character, X))
  ## Convert character vectors to factors to avoid loosing levels
  ## when calculating conditional probabilities
  if (length(charvar) > 0) {
    for (col in charvar) data.table::set(X, j = col, value = factor(X[[col]]))
  }
  xtabs0 <- function(counts, x, prop = FALSE, ...) {
    res <- stats::xtabs(counts ~ x)
    if (prop) res <- res / sum(res)
    return(structure(as.numeric(res), names = names(res)))
  }
  cls <- levels(y)
  prior0 <- xtabs0(weights, y, prop = TRUE)
  if (!is.null(prior)) {
    warning("prior argument is not implemented and has no effect.")
    ## user-defined priors
    ## TODO: Assign new values and renormalize
  }
  estcond <- function(x, weights, ...) {
    if (data.table::is.data.table(x)) x <- as.matrix(x[, 1])
    w <- weights / sum(weights)
    if (is.numeric(x)) {
      if (!kernel) {
        ## TODO: "smoothing" in sparse cases
        est <- c(mean = sum(x * w), sd = (sum(x^2 * w) - sum(x * w)^2)^.5)
        return(list(model = "gaussian", estimate = est))
      } else {
        ## Kernel density estimate
        ## TODO: add tuning parameters?
        return(list(
          model = "density",
          estimate = stats::density(x, weights = w)
        ))
      }
    } else {
      ## Laplace smoothing, (x+laplace.smooth)/(N+k*alpha),
      ## x: counts in different categories;
      ## N: total counts; k: number of categories
      ## idx <- rep(seq_along(x),weights);
      M <- xtabs0(weights, x) + laplace.smooth
      return(list(model = "multinomial", estimate = M / sum(M)))
    }
  }
  pcond <- lapply(cls, function(i) {
    idx <- which(y == i)
    m0 <- as.data.frame(X[idx, predictor, with = FALSE, drop = FALSE])
    return(lapply(m0, estcond, weights = weights[idx]))
  })
  res <- structure(
    list(
      prior = prior0, ## Pr(class)
      conditional = pcond, ## Pr(x|class)
      classes = cls,
      xvar = names(pcond[[1]]),
      xmodel = unlist(lapply(pcond[[1]], function(x) x$model)),
      design = des,
      call = match.call()
    ),
    class = "naivebayes"
  )
  return(res)
}

#' @export
print.naivebayes <-
function(x, ...) {
    print(x$call)
    cat("\n")
    val <- x$prior
    names(val) <- paste0(seq_len(length(val)), ": ", names(val))
    print(data.table::data.table(Prior=val))
    cat("\n")
}

#' Naive Bayes Classifier predictions
#' @title Predictions for Naive Bayes Classifier
#' @param object density object
#' @param newdata new data on which to make predictions
#' @param expectation Variable to calculate conditional expectation wrt
#' probabilities from naivebayes classifier
#' @param threshold Threshold parameters. First element defines the threshold
#' on the probabilities and the second element the value to set those
#' truncated probabilities to.
#' @param ... Additional arguments to lower level functions
#' @export
#' @author Klaus K. Holst
predict.naivebayes <- function(object, newdata, # nolint
                       expectation = NULL,
                       threshold = c(1e-3, 1e-3), ...) {
  if (missing(newdata)) stop("Need new data to make predictions")
  if (!is.data.table(newdata)) newdata <- data.table::data.table(newdata)
  ## Likelihood P(class|x) = P(class)P(x|class)/P(x)
  if (!is.null(expectation)) {
    if (inherits(expectation, "formula")) {
      expectation <- all.vars(expectation)
    }
    z <- newdata[, expectation] # nolint
    ## TODO: Not used for now
  }
  if (!all(c(object$model$predictor, expectation) %in% names(newdata))) {
    stop("Variables missing in data")
  }

  if (is.null(expectation)) {
    lposterior <- matrix(nrow = nrow(newdata), ncol = length(object$classes))
  }
  predictor <- object$design$term.labels
  X <- newdata[, predictor, with = FALSE, drop = FALSE]
  charvar <- names(Filter(is.character, X))
  if (length(charvar) > 0) {
    for (col in charvar) data.table::set(X, j = col, value = factor(X[[col]]))
  }
  px <- rep(0, nrow(newdata))
  for (i in seq_along(object$classes)) {
    # P(x|c) = prod P(xi|c) pr independence assumption
    lpcond <- rep(0, nrow(newdata))
    for (j in seq_along(predictor)) {
      x0 <- object$conditional[[i]]
      nam <- object$xvar[j]
      x <- as.matrix(X[, nam, with = FALSE, drop = FALSE])[, 1]
      estx <- x0[[j]]
      if (is.list(estx)) {
        estx <- estx$estimate
      }
      curmodel <- object$xmodel[j]
      if (curmodel == "multinomial") {
        xs <- unique(x)
        misx <- which(!(xs %in% names(estx)))
        if (length(misx) > 0) {
          nn <- c(names(estx), xs[misx])
          estx <- c(estx, rep(NA, length(misx)))
          names(estx) <- nn
        }
        estx[estx < threshold[1] | is.na(estx)] <- threshold[2]
        estx <- estx / sum(estx)
        lpcond <- lpcond + log(estx[x])
      }
      if (curmodel == "gaussian") {
        ## TODO: treshold
        if (is.na(estx[1])) estx[1] <- 0
        if (is.na(estx[2]) || estx[2] < 1e-16) estx[2] <- 1
        lpcond <- lpcond + dnorm(x, mean = estx[1], sd = estx[2], log = TRUE)
      }
      if (curmodel %in% c("kernel", "density")) {
        estx <- predict(estx, x)
        ## TODO: treshold
        lpcond <- lpcond + log(estx)
      }
    }
    logjoint <- lpcond + log(object$prior[i]) ## log P(x,c)
    if (is.null(expectation)) {
      lposterior[, i] <- logjoint
    }
    px <- px + exp(logjoint) ## P(x)
    lposterior[, i] <- logjoint
  }

  if (is.null(expectation)) {
    for (i in seq_len(ncol(lposterior))) {
      lposterior[, i] <- lposterior[, i] - log(px) ## log P(c|x)
    }
  }
  colnames(lposterior) <- object$classes
  return(exp(lposterior))
}


#' @title naivebayes class object
#'
#' @description The functions [naivebayes] returns an object of the type
#'   \code{naivebayes}.
#'
#' An object of class '\code{naivebayes}' is a list with at least
#' the following components:
#'
#' \describe{
#'   \item{prior}{Matrix with prior probabilities,
#' i.e. marginal class probabilities Pr(class)}
#'   \item{pcond}{list of matrices with conditional probabilities
#' of the features given the classes (one list element per class), Pr(x|class)}
#'   \item{classes}{Names (character vector) of the classes}
#'   \item{xvar}{Names of predictors}
#'   \item{xmodel}{Conditional model for each predictor}
#'   \item{design}{Model design object}
#'   \item{call}{The function call which instantiated the object}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object
#' of class \code{naivebayes}:
#' \describe{
#'   \item{\code{predict}}{Predict class probabilities for new features data.}
#'   \item{\code{print}}{Basic print method.}
#'  }
#'
#' @aliases naivebayes-class
#' @seealso [naivebayes()]
#' @return objects of the S3 class '\code{naivebayes}'
#' @examples ## See example(naivebayes) for examples
#' @docType class
#' @name naivebayes-class
NULL
