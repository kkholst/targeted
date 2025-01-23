NB_Xprep <- function(X, xlev, ...) { ## Input is data.table
    if (!data.table::is.data.table(X)) X <- data.table::data.table(X)
    charvar <- names(Filter(is.character, X))
    xlev <- rep(0, ncol(X))
    if (length(charvar)>0) {
        for (col in charvar) {
            data.table::set(X, j=col, value=factor(X[[col]]))
        }
    }
    factors <- names(Filter(is.factor, X))
    xidx <- which(names(X)%in%factors)
    levs <- vector("list", ncol(X))
    for (i in xidx) {
        levs[[i]] <- levels(X[[i]])
        xlev[i] <- nlevels(X[[i]])
        data.table::set(X, j=i, value=as.numeric(X[[i]])-1)
    }
    X <- as.matrix(X)
    return(structure(X, nlevels=xlev, levels=levs))
}

#' @export
NB2 <- function(formula, data, weights=NULL,
         laplace.smooth=0, y, x, ...) {
    if (missing(y)) {
        if (missing(data)) stop("Need data as data.frame or data.table")
        ff <- procform(formula, data=as.data.frame(data))
        data <- data.table::data.table(data)
        y <- as.factor(as.matrix(with(ff, data[, response, with=FALSE])))
        if (length(ff$filter)>0 && is.null(weights)) {
            weights <- as.matrix(data[, ff$filter[[1]], with=FALSE])[, 1]
        } else if (is.null(weights)) weights <- rep(1, length(y))
        X <- NB_Xprep(data[, ff$predictor, with=FALSE, drop=FALSE])
    } else {
        if (is.null(weights)) weights <- rep(1, length(y))
        y <- as.factor(as.matrix(y))
        X <- NB_Xprep(x)
        ff <- NULL
    }
    xlev <- attr(X, "nlevels")
    cls <- levels(y)
    ylev <- seq(nlevels(y))
    y <- as.numeric(y)
    pcond <- .NB(y, X, xlev, ylev, weights, laplace.smooth)

    xtabs0 <- function(counts, x, prop=FALSE, ...) {
        res <- stats::xtabs(counts~x)
        if (prop) res <- res/sum(res)
        return(structure(as.numeric(res), names=names(res)))
    }
    prior0 <- xtabs0(weights, y, prop=TRUE)
    res <- list(prior=prior0,
               conditional=pcond,
               classes=cls,
               model=ff,
               xvar=colnames(X),
               xlevels=attr(X, "levels"),
               xmodel=ifelse(xlev>0, "multinomial", "gaussian"),
               call=match.call())
    return(structure(res, class=c("NB2", "NB")))
}


#' @export
predict.NB2 <- function(object,newdata, threshold=1e-3, ...) {
    if (missing(newdata)) stop("Need new data to make predictions")
    if (!data.table::is.data.table(newdata)) newdata <- data.table::data.table(newdata)
    if (!is.null(object$model)) {
      if (!all(c(object$model$predictor)%in%names(newdata))) stop("Variables missing in data")
      xx <- object$model$predictor
      X <-  NB_Xprep(newdata[, xx, with=FALSE, drop=FALSE])
    } else {
      if (all(object$xvar%in%names(newdata))) {
        X <-  NB_Xprep(newdata[,object$xvar,with=FALSE,drop=FALSE])
      } else {
        X <-  NB_Xprep(newdata)
      }
    }
    lev <- attr(X, "levels")
    xord <- vector("list", length(lev))
    for (i in seq_along(xord)) {
        if (!is.null(lev[[i]])) {
            xord[[i]] <- match(lev[[i]], object$xlevels[[i]])-1
        } else {
          xord[[i]] <- 0  # to make sure that xord can be interpreted as std::vector<arma::vec>
        }
    }
    ll <- unlist(object$conditional, recursive=FALSE)
    ## conditional prob:
    lp <- .predNB(X, ll,
                  xord, (object$xmodel=="multinomial")*1L,
                  object$prior, threshold)
    colnames(lp) <- object$classes
    exp(lp)
}
