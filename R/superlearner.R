metalearner_nnls <- function(y, pred, method = "nnls") {
  if (NCOL(pred)==1) return(1.0)
  if (method == "nnls") {
    res <- nnls::nnls(A = pred, b = y)
    coefs <- res$x
  } else {
    res <- glmnet::glmnet(
      y = y, x = pred,
      intercept = FALSE,
      lambda = 0,
      lower.limits = rep(0, ncol(pred))
    )
    coefs <- as.vector(coef(res))[-1]
  }
  if (any(is.na(coefs))) coefs[is.na(coefs)] <- 0
  if (all(coefs == 0)) coefs[1] <- 1
  return(coefs / sum(coefs))
}

get_learner_names <- function(model.list, name.prefix) {
  .names <- names(model.list)
  if (is.null(.names)) .names <- rep("", length(model.list))

  if (is.null(name.prefix)) {
    # NULL check because learner$new has info = NULL by default
    new_names <- lapply(
      model.list,
      \(lr) ifelse(is.null(lr$info), "", lr$info)
    ) |> unlist()
  } else {
    new_names <- paste0(name.prefix, seq_along(model.list))
  }
  .names[.names == ""] <- new_names[.names == ""]
  return(.names)
}

#' @export
#' @title Superlearner (stacked/ensemble learner)
#' @description This function creates a predictor object (class [learner])
#'   from a list of existing [learner] objects. When estimating this model a
#'   stacked prediction will be created by weighting together the predictions
#'   of each of the initial learners The weights are learned using
#'   cross-validation.
#' @param data (data.frame) Data containing the response variable and
#' covariates.
#' @param learners (list) List of [learner] objects (i.e. [predictor_glm])
#' @param nfolds (integer) Number of folds to use in cross-validation to
#' estimate the ensemble weights.
#' @param meta.learner (function) Algorithm to learn the ensemble
#' weights (default non-negative least squares). Must be a function of the
#' response (nx1 vector), `y`, and the predictions (nxp matrix), `pred`, with
#' p being the number of learners.
#' @param model.score (function) Model scoring method (see [learner])
#' @param name.prefix (character) Prefix used to name learner objects in
#' `learners` without names. If NULL, then obtain the name from the info field
#' of a learner.
#' @param mc.cores (integer) If not NULL, then [parallel::mcmapply] is used with
#' `mc.cores` number of cores for parallelization instead of the
#' [future.apply::future_lapply] package. Parallelization is disabled with
#' `mc.cores = 1`.
#' @param silent (logical) Suppress all messages and progressbars
#' @param future.seed (logical or integer) Argument passed on to
#' [future.apply::future_lapply]. If TRUE, then [.Random.seed] is used if it
#' holds a L'Ecuyer-CMRG RNG seed, otherwise one is created randomly.
#' @param ... Additional arguments to [parallel::mclapply] or
#' [future.apply::future_lapply].
#' @references Luedtke & van der Laan (2016) Super-Learning of an Optimal
#'   Dynamic Treatment Rule, The International Journal of Biostatistics.
#' @seealso [predict.superlearner] [weights.superlearner] [score.superlearner]
#' @examples
#' sim1 <- function(n = 5e2) {
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    data.frame(y, x1, x2)
#' }

#' m <- list(
#'   "mean" = predictor_glm(y ~ 1),
#'   "glm" = predictor_glm(y ~ x1 + x2)
#' )
#' sl <- superlearner(m, data = sim1(), nfolds = 2)
#' predict(sl, newdata = sim1(n = 5))
#' predict(sl, newdata = sim1(n = 5), all.learners = TRUE)
superlearner <- function(learners,
                         data,
                         nfolds = 10,
                         meta.learner = metalearner_nnls,
                         model.score = mse,
                         mc.cores = NULL,
                         future.seed = TRUE,
                         silent = TRUE,
                         name.prefix = NULL,
                         ...) {
  # TODO: test that response of all learners is the same
  pred_mod <- function(models, data) {
    res <- lapply(models, \(x) x$predict(data))
    return(Reduce(cbind, res))
  }
  if (is.character(model.score)) {
    model.score <- get(model.score)
  }

  if (any(!unlist(lapply(learners, \(lr) inherits(lr, "learner"))))) stop(
    "All provided learners must be of class targeted::learner"
  )

  model.names <- get_learner_names(learners, name.prefix)
  n <- nrow(data)
  folds <- lava::csplit(n, nfolds)
  pred <- matrix(NA, n, length(learners))
  if (!silent) pb <- progressr::progressor(along = seq_len(nfolds))
  onefold <- function(fold, data, learners, pb) {
    n <- nrow(data)
    test <- data[fold, , drop = FALSE]
    train <- data[setdiff(1:n, fold), , drop = FALSE]
    mod <- lapply(learners, \(x) x$clone(deep = TRUE))
    lapply(mod, \(x) x$estimate(train))
    pred.test <- pred_mod(mod, test)
    if (!silent) pb()
    return(list(pred = pred.test, fold = fold))
  }
  if (!is.null(mc.cores)) {
    if (mc.cores == 1L) {
      ## disable parallelization
      pred.folds <- lapply(folds, function(fold) {
        return(onefold(fold, data, learners, pb))
      })
    } else {
      ## mclapply
      pred.folds <- parallel::mclapply(
        folds,
        function(fold) onefold(fold, data, learners, pb),
        mc.cores = mc.cores, ...
        )
    }
  } else {
    ## future
    pred.folds <- do.call(
      future.apply::future_lapply,
      list(
        X = folds,
        FUN = function(fold) onefold(fold, data, learners, pb),
        future.seed = future.seed,
        ...
      )
    )
  }
  for (i in seq_along(pred.folds)) {
    pred[pred.folds[[i]]$fold, ] <- pred.folds[[i]]$pred
  }
  mod <- lapply(learners, \(x) x$clone())
  names(mod) <- model.names
  ## Meta-learner
  y <- learners[[1]]$response(data)
  risk <- apply(pred, 2, \(x) model.score(y, x))
  names(risk) <- model.names
  w <- meta.learner(y = y, pred = pred)
  names(w) <- model.names
  ## Full predictions
  lapply(mod, \(x) x$estimate(data))
  res <- list(
    model.score = risk,
    weights = w,
    names = model.names,
    fit = mod,
    folds = folds
  )
  return(structure(res, class = "superlearner"))
}

#' @export
print.superlearner <- function(x, ...) {
  res <- cbind("score" = x$model.score, "weight" = x$weights)
  if (!is.null(x$fit)) {
      rownames(res) <- x$names
  } else {
    rownames(res) <- paste("model", seq_along(x$fit))
  }
  return(print(res))
}


#' @title Extract ensemble weights
#' @param object (superlearner) Fitted model.
#' @export
weights.superlearner <- function(object, ...) {
  return(object$weights)
}

#' @title Extract average cross-validated score of individual learners
#' @param x (superlearner) Fitted model.
#' @export
score.superlearner <- function(x, ...) {
  return(x$model.score)
}


#' @title Predict Method for superlearner Fits
#' @description Obtains predictions for ensemble model or individual learners.
#' @export
#' @param object (superlearner) Fitted [superlearner] object.
#' @param newdata (data.frame) Data in which to look for variables with which to
#' predict.
#' @param all.learners (logical) If FALSE (default), then return the predictions
#' from the ensemble model. Otherwise, return predictions of from all individual
#' learners.
#' @param ... Not used.
#' @return numeric (`all.learners = FALSE`) or matrix (`all.learners = TRUE`)
predict.superlearner <- function(object, newdata, all.learners = FALSE, ...) {
  pr <- lapply(object$fit, \(x) x$predict(newdata))
  res <- Reduce(cbind, pr)
  colnames(res) <- names(object$fit)

  if (!all.learners) {
    res <- as.vector(res %*% object$weights)
  }
  return(res)
}

#' SuperLearner wrapper for learner
#'
#' @title SuperLearner wrapper for learner
#' @aliases SL
#' @param formula Model design
#' @param ... Additional arguments for SuperLearner::SuperLearner
#' @param SL.library character vector of prediction algorithms
#' @param binomial boolean specifying binomial or gaussian family (default
#'   FALSE)
#' @param data Optional data.frame
#' @param info model information (optional)
#' @return learner object
#' @author Klaus Kähler Holst
#' @export
SL <- function(formula=~., ...,
               SL.library=c("SL.mean", "SL.glm"),
               binomial=FALSE,
               data=NULL,
               info = "SuperLearner") {
  dots <- list(...)
  if (!requireNamespace("SuperLearner")) {
      stop("Package 'SuperLearner' required.")
  }
  pred <- as.character(formula)
  pred <- ifelse(length(pred)==2, pred[2], pred[3])
  if (pred=="1") {
    SL.library <- "SL.mean"
  }
  m <- learner$new(formula,
    info = info,
    estimate = function(x, y) {
      Y <- as.numeric(y)
      X <- as.data.frame(x)
      args <- c(list(
        Y = Y, X = X,
        SL.library = SL.library
      ), dots)
      if (binomial) {
        args <- c(args, list(family = binomial()))
      }
      res <- do.call(SuperLearner::SuperLearner, args)
      res$call <- quote(SuperLearner(...))
      if (binomial) {
        res$call <- quote(
          SuperLearner::SuperLearner(
            ...,
            family = binomial()
          )
        )
      }
      return(res)
    },
    predict = function(object, newdata) {
      pr <- predict(object, newdata = newdata)$pred
      if (binomial) {
        pr <- cbind((1 - pr), pr)
      }
      return(pr)
    }
    )
  if (!is.null(data))
    m$estimate(data)
  return(m)
}
