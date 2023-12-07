##' Augmented Inverse Probability Weighting estimator for the Average (Causal)
##' Treatment Effect. All nuisance models are here parametric (glm). For a more
##' general approach see the \code{cate} implementation. In this implementation
##' the standard errors are correct even when the nuisance models are
##' misspecified (the influence curve is calculated including the term coming
##' from the parametric nuisance models). The estimate is consistent if either
##' the propensity model or the outcome model / Q-model is correctly specified.
##'
##' @title AIPW (doubly-robust) estimator for Average Treatement Effect
##' @param formula Formula (see details below)
##' @param data data.frame
##' @param weights optional frequency weights
##' @param offset optional offset (character or vector). can also be specified in the formula.
##' @param family Exponential family argument for outcome model
##' @param nuisance outcome regression formula (Q-model)
##' @param propensity propensity model formula
##' @param all If TRUE all standard errors are calculated (default TRUE when exposure
##' only has two levels)
##' @param labels Optional treatment labels
##' @param ... Additional arguments to lower level functions
##' @return An object of class '\code{ate.targeted}' is returned. See \code{\link{targeted-class}}
##' for more details about this class and its generic functions.
##' @details
##' The formula may either be specified as:
##' response ~ treatment | nuisance-formula | propensity-formula
##'
##' For example: \code{ate(y~a | x+z+a | x*z, data=...)}
##'
##' Alternatively, as a list: \code{ate(list(y~a, ~x+z, ~x*z), data=...)}
##'
##' Or using the nuisance (and propensity argument): \code{ate(y~a, nuisance=~x+z, ...)}
##' @export
##' @author Klaus K. Holst
##' @aliases ate
##' @examples
##' m <- lvm(y ~ a+x, a~x)
##' distribution(m,~ a+y) <- binomial.lvm()
##' d <- sim(m,1e3,seed=1)
##'
##' a <- ate(y ~ a, nuisance=~x, data=d)
##' summary(a)
##' b <- cate(a ~ 1, y ~ a*x, a ~ x, data=d)
##'
##' # Multiple treatments
##' m <- lvm(y ~ a+x, a~x)
##' distribution(m,~ y) <- binomial.lvm()
##' m <- ordinal(m, K=4, ~a)
##' transform(m, ~a) <- factor
##' d <- sim(m, 1e3, seed=1)
##' (a <- ate(y~a|a*x|x, data=d))
##'
##' # Comparison with randomized experiment
##' m0 <- cancel(m, a~x)
##' lm(y~a-1, sim(m0,1e5))
##'
##' # Choosing a different contrast for the association measures
##' summary(a, contrast=c(2,4))
ate <- function(formula,
                data=parent.frame(), weights, offset,
                family=stats::gaussian(identity),
                nuisance=NULL,
                propensity=nuisance,
                all, labels=NULL, ...) {
    cl <- match.call()
    if (!is.null(nuisance) && inherits(nuisance, "formula")) {
        exposure <- attr(lava::getoutcome(formula), "x")
        formula <- list(formula, nuisance, propensity)
        if (!(exposure%in%all.vars(nuisance)) && length(all.vars(nuisance))>0) {
            nuisance <- update(nuisance, as.formula(paste0("~ . +", exposure)))
            formula[[2]] <- nuisance
        }
    }
    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }
    if (is.list(formula)) {
        yf <- lava::getoutcome(formula[[1]])
        exposure <- gsub("`", "", attr(yf, "x"))
        xf <- formula
        xf[[1]] <- update(xf[[1]], .~.-1)
    } else {
        if (inherits(formula, "formula")) {
            yf <- lava::getoutcome(formula, sep="|")
            xf <- attr(yf, "x")
            exposure <- attr(lava::getoutcome(xf[[1]]), "x")
            xf[[1]] <- update(as.formula(paste0(yf, deparse(xf[[1]]))), .~.-1)
        }
    }
    if (is.list(formula) || inherits(formula, "formula")) {
        ## xf <- lapply(xf, function(x) { environment(x) <- baseenv(); return(x) })
        xx <- lapply(xf, function(x) model.matrix(x, data=data))
        yx <- model.frame(xf[[1]], data=data)
        ## }
        a <- yx[, exposure]
        y <- yx[, yf[1]]
        x1 <- xx[[2]]
        x2 <- xx[[3]]
        if (base::missing(offset))
          offset <- model.offset(model.frame(xf[[2]], data=data))
        nn <- list(yf[1], exposure, colnames(x1), colnames(x2))
        rm(yx, xx, yf)
    } else {
        stop("Expected a formula") # or a matrix (response,exposure)")
    }
    if (base::missing(weights) || length(weights)==0) weights <- rep(1, length(y))
    if (inherits(weights, "formula")) weights <- all.vars(weights)
    if (is.character(weights)) weights <- as.vector(data[, weights])

    if (base::missing(offset) || length(offset)==0) offset <- rep(0, length(y))
    if (inherits(offset, "formula")) offset <- all.vars(offset)
    if (is.character(offset)) offset <- as.vector(data[, offset])
    ##l1 <- glm.fit(y=y, x=x1, weights=weights, family=family)
    l1 <- glm(y ~ -1+x1, weights=weights, offset=offset, family=family)
    beta <- coef(l1)
    names(beta) <- gsub("^x1", "" ,names(beta))
    ## iid.beta <- fast_iid(y, l1$fitted, x1, weights, logistic=family$family=="binomial")/length(y)
    iid.beta <- IC(l1)/length(y)
    treatments <- if (is.factor(a)) levels(a) else sort(unique(a))
    if (length(treatments)>20) stop("Unexpected large amount of treatments")
    if (base::missing(all)) all <- length(treatments)==2
    if (length(treatments)>2) {
        if (!is.factor(a)) warning("`", exposure, "` should probably be converted into a factor. An additive model is now assumed in the outcome regression model.")
    }
    est <- c()
    iids <- matrix(nrow=length(y), ncol=length(treatments))
    coefs <- numeric(length(treatments))
    count <- 0
    data0 <- data
    bprop <- NULL
    Vprop <- NULL
    nlabels <- labels
    for (trt in treatments) {
        count <- count+1
        a0 <- (a==trt)
        ## For simplicity we here fit a logistic regression for each treatment.
        ## TODO: we do not need to recalculate logistic regression for the last treatment.
        l2 <- glm.fit(y=a0, x=x2, weights=weights, family=binomial("logit"))
        gamma <- l2$coef
        data0[, exposure] <- factor(trt, levels=treatments)
        x1 <- model.matrix(xf[[2]], data=data0)
        val <- ace_est(y=cbind(y), a=cbind(a0), x1=cbind(x1), x2=cbind(x2),
                       theta=c(beta, gamma), weights=weights, offset=offset, link=family$link)
        alpha.index <- 1
        beta.index <- seq_along(beta) + length(alpha.index)
        gamma.index <- seq_along(gamma) + length(alpha.index) + length(beta.index)
        U0 <- val$u
        DU <- t(val$du)
        iid.gamma <- fast_iid(a0, l2$fitted, x2, weights)/length(a0)
        if (count==1) {
            gidx <- seq_along(gamma)
            bprop <- numeric(length(gamma)*(length(treatments)-1))
            names(bprop) <- rep(names(gamma), length(treatments)-1)
            Vprop <- matrix(NA, ncol=length(bprop), nrow=length(bprop))
        }
        if (count < length(treatments) && all) {
            gpos <- gidx+length(gamma)*(count-1)
            bprop[gpos] <- gamma
            Vprop[gpos, gpos] <- crossprod(iid.gamma)
        }
        iid.alpha <- (U0 + iid.beta %*% t(DU[, beta.index, drop=FALSE]) +
                       iid.gamma %*% t(DU[, gamma.index, drop=FALSE])) %*%
            t(-Inverse(DU[, alpha.index, drop=FALSE]))
        iids[, count] <- iid.alpha
        coefs[count] <- val$alpha
        if (is.null(labels)) nlabels <- c(nlabels, paste0(exposure, "=", trt))
    }
    Vout  <- NULL
    if (all) Vout <- crossprod(iid.beta)
    outreg <- lava::estimate(coef=beta, vcov=Vout)
    propmod <- lava::estimate(coef=bprop, vcov=Vprop)
    V <- crossprod(iids)
    est <- lava::estimate(coef=coefs, vcov=V, labels=nlabels)
    est$IC <- iids * NROW(iids)
    rownames(est$IC) <- rownames(data)
    structure(list(estimate=est,
                   outcome.reg=outreg, propensity.model=propmod, names=unlist(nn)[1:2],
                   formula=xf,
                   npar=c(length(treatments), ncol(x1), ncol(x2)), nobs=length(y), opt=NULL,
                   all=all,
                   family=family),
              class=c("ate.targeted", "targeted"))
}


##' @export
print.summary.ate.targeted <- function(x, ...) {
    nam <- x$names
    cat("\nAugmented Inverse Probability Weighting estimator\n")
    outreg <- x$family$family
    cat("  Response ", nam[[1]], " (Outcome model: ", outreg, "):\n", sep="")
    cat("\t", paste(nam[[1]], x$formula[[2]]), "\n")
    cat("  Exposure ", nam[[2]], " (Propensity model: logistic regression):\n", sep="")
    cat("\t", paste(nam[[2]], x$formula[[3]]), "\n")
    cat("\n")
    if (x$all) {
        sep_which <- x$npar[[1]]
        sep_labels <- NULL
        if (x$npar[[2]]>0) {
            sep_labels <- nam[[4]]
            if (x$npar[[3]]>0)
                sep_which <- c(sep_which, x$npar[[1]]+x$npar[[2]])
        }
        if (x$npar[[3]]>0)
            sep_labels <- c(sep_labels, nam[[5]])
        print(x$estimate, unique.names=FALSE,
              sep.which=sep_which,
              sep.labels=sep_labels, ...)
    } else {
        print(x$estimate, unique.names=FALSE, ...)
    }
    if (length(x$contrast)>1) {
        cc <- rownames(x$estimate$coefmat)
        with(x, cat("\nAverage Treatment Effect (constrast: '",
                    cc[contrast[1]], "' - '", cc[contrast[2]], "'):\n\n", sep=""))
        if (!is.null(x$asso)) print(x$asso)
    }
    cat("\n")
}


##' @export
summary.ate.targeted <- function(object, contrast=c(2:1), ...) {
  nn <- lapply(object[c("estimate", "outcome.reg", "propensity.model")],
               function(x) length(coef(x)))
    if (object$all) {
        cc <- rbind(object$estimate$coefmat,
                    object$outcome.reg$coefmat,
                    object$propensity.model$coefmat)
    } else {
        cc <- object$estimate$coefmat
    }
    cc <- lava::estimate(coef=cc[, 1], vcov=diag(cc[, 2]^2, ncol=nrow(cc)), labels=rownames(cc))
    if (length(contrast)>2)
        warning("Only the first two elements of 'contrast' are used")
    if (object$npar[1]<2) contrast <- 1
    asso <- NULL
    if (length(contrast)>=2)
        if (object$family$family=="binomial") {
          asso <- estimate(object$estimate, function(x) c(x[contrast[1]]/x[contrast[2]],
                                                          lava::OR(x[contrast]),
                                                          x[contrast[1]]-x[contrast[2]]),
                           labels=c("RR", "OR", "RD"))
        } else {
          asso <- estimate(object$estimate, function(x) x[contrast[1]]-x[contrast[2]],
                           labels=c("ATE"))
        }
  structure(list(estimate=cc, npar=nn, type=object$type, asso=asso,
                 family=object$family,
                 names=c(object$names, "", "Outcome model:", "Propensity model:"),
                 all=object$all, formula=gsub("~", "~ ", unlist(lapply(object$formula, deparse))),
                 contrast=contrast),
            class="summary.ate.targeted")
}
