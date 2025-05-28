#' @description Constructs a [learner] class object for fitting a
#' [superlearner].
#' @export
#' @inherit constructor_shared
#' @inheritParams superlearner
#' @seealso [cv.learner_sl]
#' @param ... Additional arguments to [superlearner]
#' @examples
#' sim1 <- function(n = 5e2) {
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    data.frame(y, x1, x2)
#' }
#' d <- sim1()
#'
#' m <- list(
#'   "mean" = learner_glm(y ~ 1),
#'   "glm" = learner_glm(y ~ x1 + x2),
#'   "iso" = learner_isoreg(y ~ x1)
#' )
#'
#' s <- learner_sl(m, nfolds = 10)
#' s$estimate(d)
#' pr <- s$predict(d)
#' if (interactive()) {
#'     plot(y ~ x1, data = d)
#'     points(d$x1, pr, col = 2, cex = 0.5)
#'     lines(cos(x1) + x1 ~ x1, data = d[order(d$x1), ],
#'           lwd = 4, col = lava::Col("darkblue", 0.3))
#' }
#' print(s)
#' # weights(s$fit)
#' # score(s$fit)
#'
#' cvres <- cv(s, data = d, nfolds = 3, rep = 2)
#' cvres
#' # coef(cvres)
#' # score(cvres)
learner_sl <- function(learners,
                       info = NULL,
                       nfolds = 5L,
                       meta.learner = metalearner_nnls,
                       model.score = mse,
                       learner.args = NULL,
                       ...) {
  if (is.null(info)) {
    info <- "superlearner\n"
    nn <- names(learners)
    for (i in seq_along(nn)) {
      info <- paste0(info, "\t", nn[i])
      if (i < length(nn)) info <- paste0(info, "\n")
    }
  }
  args <- c(learner.args, list(info = info))
  estimate.args <- list(learners = learners, nfolds = nfolds,
    meta.learner = meta.learner, model.score = model.score
  )
  args$estimate.args <- c(estimate.args, list(...))
  args$estimate <- function(data, ...) superlearner(data = data, ...)
  args$predict <- function(object, newdata, ...) predict(object, newdata, ...)

  mod <- do.call(learner$new, args)

  # duplicate check from superlearner to catch error during instantiation
  # of a learner instead in the estimate method call
  if (length(unique(lapply(learners, \(m) all.vars(m$formula)[[1]]))) > 1) {
    stop("All learners must have the same response variable.")
  }
  mod$update(learners[[1]]$formula) # TODO: not clean but will be fixed later
  # because it requires changes to the learner R6 class and the cv function

  attr(mod, "model.score") <- model.score
  class(mod) <- c("learner_sl", class(mod))
  return(mod)
}

score_sl <- function(response,
                     newdata,
                     object,
                     model.score,
                     ...) {
  pr.all <- object$predict(newdata, all.learners = TRUE)
  pr <- object$predict(newdata)
  risk.all <- apply(pr.all, 2, function(x) model.score(x, response))
  risk <- cbind(rbind(model.score(response, pr))[1, ])
  nam <- names(risk)
  if (is.null(nam)) nam <- "score"
  nam <- paste0(nam, ".")
  risk <- cbind(risk, rbind(risk.all))
  colnames(risk)[1] <- "sl"
  nn <- colnames(risk)
  names(risk) <- paste0(nam, nn)
  w <- rbind(c(NA, weights(object$fit)))
  rownames(w) <- "weight"
  risk <- rbind(risk, w)
  res <- c()
  for (i in seq_len(nrow(risk))) {
    x <- risk[i, ]
    names(x) <- paste0(rownames(risk)[i], ".", colnames(risk), sep="")
    res <- c(res, x)
  }
  return(res)
}

#' Cross-validation for [learner_sl]
#' @description Cross-validation estimation of the generalization error of the
#'   super learner and each of the separate models in the ensemble. Both the
#'   chosen model scoring metrics as well as the model weights of the stacked
#'   ensemble.
#' @param object (learner_sl) Instantiated [learner_sl] object.
#' @export
#' @inheritParams cv.default
#' @examples
#' sim1 <- function(n = 5e2) {
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    data.frame(y, x1, x2)
#' }
#' sl <- learner_sl(list(
#'                    "mean" = learner_glm(y ~ 1),
#'                    "glm" = learner_glm(y ~ x1),
#'                    "glm2" = learner_glm(y ~ x1 + x2)
#'                   ))
#' cv(sl, data = sim1(), rep = 2)
cv.learner_sl <- function(object,
                            data,
                            nfolds = 5,
                            rep = 1,
                            model.score = scoring,
                            ...) {
  res <- cv(list("performance"=object),
            data = data,
            nfolds = nfolds, rep = rep,
            model.score = function(...) score_sl(..., model.score = model.score)
            )
  nam <- dimnames(res$cv)
  nam <- nam[[length(nam)]]
  st <- strsplit(nam, "\\.")
  type <- unlist(lapply(st, \(x) x[1])) |> unique() # metrics
  n <- length(nam)/length(type) # number of models
  nam <- gsub(paste0(type[1], "\\."), "", nam[seq_len(n)])

  idx <- 1:n
  cvs <- c()
  for (i in seq_len(length(type))) {
    score <- res$cv[, , , idx + (i-1)*n, drop=FALSE]
    cvs <- abind::abind(cvs, score, along=3)
  }
  dimnames(cvs)[[4]] <- nam
  dimnames(cvs)[[3]] <- type
  cvs <- aperm(cvs, c(1, 2, 4, 3))
  res$names <- nam
  res$cv <- cvs
  res$call <- NULL
  class(res) <- c("cross_validated.learner_sl", "cross_validated")
  return(res)
}

#' @export
print.cross_validated.learner_sl <- function(x, digits=5, ...) {
  res <- round(summary.cross_validated(x)*1e5, digits=0) / 1e5
  cat("\n", x$fold, "-fold cross-validation", sep="")
  if (x$rep > 1) cat(" with ", x$rep, " repetitions", sep="")
  cat("\n")
  p <- dim(res)[3]
  for (i in seq_len(p)) {
    cli::cli_h3(dimnames(res)[[3]][i])
    print(res[, , i], na.print="-")
  }
}
