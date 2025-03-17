#' @title Hyperparameter tuning
#' @param model [ml_model] object
#' @param data data.frame
#' @param nfolds Number of folds to use in cross-validation
#' @param model.score model scoring (default MSE)
#' @param ... Additional arguments to
#' [rBayesianOptimization::BayesianOptimization]
#' @return list
#' @inherit rBayesianOptimization::BayesianOptimization
#' @author Klaus Kähler Holst
#' @export
#' @aliases tune tuner
tune <- function(model, bounds, data,
                 n_iter = 15,
                 kappa = 2.576,
                 init_points = 4,
                 nfolds = 5,
                 model.score = targeted::scoring,
                 ...) {
  obj <- function(...) {
    cl <- rlang::call_match(defaults = TRUE)
    cl[1] <- expression(model)
    f <- eval(cl)
    res <- targeted::cv(list(f),
      model.score = model.score,
      nfolds = nfolds,
      data = data,
      silent = TRUE
    )
    list(Score = -coef(res)[1], Pred = 0)
  }
  bo.args <- c(list(
    FUN = obj,
    bounds = bounds,
    init_points = init_points,
    n_iter = n_iter,
    kappa = kappa
  ), list(...))
  arg <- names(bounds)
  idx.cat <- which(unlist(lapply(bounds, is.list)))
  if (length(idx.cat) > 0) {
    par.cat <- arg[idx.cat]
  }
  res <- c()
  if (length(par.cat) == 0) {
      op <- do.call(rBayesianOptimization::BayesianOptimization, bo.args)
      res <- list(op)
  } else {
    par.cat.grid <- expand.grid(bounds[idx.cat])
    bounds[idx.cat] <- NULL
    bo.args$bounds <- bounds
      colnames(par.cat.grid) <- par.cat
      for (i in seq_len(nrow(par.cat.grid))) {
        obj0 <- obj
        formals(obj0)[par.cat] <- par.cat.grid[i, ]
        bo.args$FUN <- obj0
        op <- do.call(rBayesianOptimization::BayesianOptimization, bo.args)
        for (j in seq_len(length(par.cat))) {
          op$History[[par.cat[j]]] <-
            par.cat.grid[i, j]
        }
        res <- c(res, list(op))
      }
  }
  res <- Reduce(rbind, lapply(res, function(x) x$History)) |>
    as.data.frame()
  ii <- which.max(res$Value)
  par.opt <- res[ii, arg, drop=TRUE]
  mod <- do.call(model, par.opt)
  mod$estimate(data)
  return(list(
    history = res,
    model = mod,
    value = -res$Value[ii],
    best = par.opt)
    )
}

#' @export
tuner <- function(mod, bounds, data, ...) {
  modwrap <- function(...) {
    args <- list(...)
    mod$args[names(args)] <- args
    mod$description <- ""
    return(mod)
  }
  tune(modwrap, bounds=bounds, data=data, ...)
}
