################################################################################

#' @title condlogit class object
#'
#' @description The functions \code{\link{condlogit}} returns an object of the type \code{condlogit}.
#'
#' An object of class '\code{condlogit}' is a list with at least the following components:
#' \describe{
#'   \item{coef}{An \code{estimate} object with the target parameter estimates (see \code{\link[lava]{estimate.default}})}
#'   \item{vcov}{An \code{estimate} object with the target parameter estimates (see \code{\link[lava]{estimate.default}})}
#'   \item{opt}{Object returned from the applied optimization routine}
#'   \item{idx}{number of parameters of the model (target and nuisance)}
#'   \item{alternatives}{String describing the model}
#' \item{alternatives}{String describing the model}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object of class \code{targeted}:
#' \itemize{
#'   \item{\code{coef}}{Extract estimated model coefficients.}
#'   \item{\code{vcov}}{Extract the variance-covariance matrix of the parameters.}
#'   \item{\code{iid}}{Extract the estimated influence function.}
#'   \item{\code{print}}{Print parameter estimates.}
#'   \item{\code{summary}}{Extract  model.}'
#'  }
#'
#' @aliases condlogit-class
#' @return objects of the S3 class '\code{condlogit}'
#' @examples ## See example(condlogit) for examples
#' @docType class
#' @name condlogit-class
NULL

################################################################################

##' Conditional (multinomial) logit model for discrete choice experiments (McFadden 1974)
##' @title Conditional logit model
##' @param choice Vector of choices (1,0) for each alternative or string with column-name in the data. Alternatively, a formula (see details below).
##' @param x Individual specific covariates. Matrix or formula.
##' @param z1 Alternative specific covariates (constant effect on utility within each alternative). Matrix or formula.
##' @param z2 Alternative specific covariates (effect on utility varies for each alternative). Matrix or formula.
##' @param weights frquency weights (vector or column-name)
##' @param alt Vector of alternatives or character string with the column-name
##' @param id Vector of individual identifiers or character string with the column-name
##' @param data data.frame
##' @param reference Base alternative (reference)
##' @param control Optimization arguments
##' @param method Type of optimization
##' @param lambda2 l2 regularization
##' @param debug debug
##' @param ... Additional arguments to lower level functions
##' @author Klaus K. Holst
##' @details  'choice' can be specified as formula with the following syntax:
##' `choice ~ alternatives | id` thus specifying both 'choice', 'alt', and 'id' which are automatically extracted from the data.frame.
##' The data.frame is in this case assumed to be on a long format where each row represents the choice and features for each alternative.
##' The set of alternatives may vary between individuals.  Each type of covariate should then also be defined by separate formulas, e.g.,
##' `x=1+x1+x2`, `z1=~z1`, `z2=~z2` (some potentially omitted).
##'
##' It is also possible to omit the "bar" notation, e.g., using the syntax
##' `chosen_alternative ~ 1+x1+x2 ...`. In this case the data is assumed to be in short form where there are no alternative specific covariates (z1=z2=NULL),
##' and the data.frame only contains the rows where `choice==1` (similar to for example mets::mlogit or nnet::multinom).
##' @return
##' Object of type \code{condlogit}.
##' @examples
##' data(iris)
##' g <- condlogit(Species ~ Sepal.Width*Petal.Length, data=iris)
##' pr <- predict(g, newdata=iris, wide=TRUE)
##' cl <- colnames(pr)[apply(pr, 1, which.max)]
##' table(iris$Species, cl)
##' @export
condlogit <- function(choice=NULL,
               x=~1,
               z1=~0,
               z2=~0,
               weights=NULL,
               alt=NULL,
               id=NULL,
               data=parent.frame(),
               reference=1,
               control=list(),
               method="nlminb",
               lambda2=0,
               debug=FALSE, ...) {
    ## Set-up
    cl <- match.call()
    env <- condlogit_prep(choice, x=x, z1=z1, z2=z2, alt=alt, id=id, weights=weights, data=data)
    idx <- env$par.idx
    theta0 <- control$start
    if (is.null(theta0)) {
        ## Starting values
        pX <- with(env$par.idx, rep(0.1,length(x)))
        pZ1 <- with(env$par.idx, rep(0.1,length(z1)))
        pZ2 <- with(env$par.idx, rep(0.1,length(z2)))
        theta0 <- c(pZ1, pZ2, pX)
    }
    M <- with(env, new(dcmodel,
                       choice, alternatives-1, cluster$idx,
                       list(z1,z2,x), weights))
    M$ref(reference-1)
    f2 <- function(theta,...) {
        M$update(theta)
        return(-M$logl())
    }
    g2 <- function(theta,...) {
        M$update(theta)
        return(-M$score(FALSE))
    }
    h2 <- function(theta,...) {
        M$update(theta)
        return(-M$hess())
    }
    ## Optimization
    if (tolower(method[1])=="nr") {
        myobj <- function(theta,...) {
            val <- obj(theta,return_hessian=TRUE,...)
            res <- -val$hess
            attr(res, "grad") <- -colSums(val$grad)
            res
        }
        op <- lava::NR(hessian=myobj, theta0, f2)
    } else {
        op <- optimx::optimx(theta0, f2, g2, h2,
                             method=method,
                             control=control)
        op1 <- head(summary(op, order="value"), 1)
        op <- list(objective=op1[,"value",drop=TRUE],
                    par=unlist(op1[,seq_along(theta0),drop=TRUE]))
        ##op <- stats::nlminb(theta0, f2, g2, h2, control=control)
        ##op <- stats::nlminb(theta0, f2, g2, control=control)
    }
    theta <- op$par
    alts <- env$levels
    alts0 <- alts[-reference]
    J <- length(alts)
    nam <- c()
    if (length(idx$z1)>0)
        nam <- with(env, c(colnames(z1)))
    sep <- ""
    if (length(idx$z2)>0) {
        if (J==2) nam <- c(nam, colnames(env$z2))
        else for (n in alts)
            nam <- c(nam, paste0(paste0("[",n,"]"),sep,colnames(env$z2)))
    }
    if (length(idx$x)>0) {
        if (J==2) nam <- c(nam, colnames(env$x))
        else for (n in alts0)
            nam <- c(nam, paste0(paste0("[",n,"]"),sep,colnames(env$x)))
    }
    names(theta) <- nam
    I <- h2(theta)
    val <- -f2(theta)
    vc <- lava::Inverse(I)
    dimnames(vc) <- list(nam,nam)

    browser()
    formulas <- NULL
    if (all(unlist(lapply(list(x,z1,z2),function(x) inherits(x,"formula"))))) {
        formulas <- list(choice=choice, x=x, z1=z1, z2=z2)
    }
    res <- list(call=cl, opt=op, coef=theta, vcov=vc, idx=idx, val=val,
                reference=reference, alternatives=env$levels,
                n=env$cl$n, nrow=length(env$choice),
                formula=formulas, order=env$ord)
    return(structure(res, class="condlogit"))
}

################################################################################

#' @title condlogit class object
#'
#' @description The functions \code{\link{condlogit}} returns an object of the type \code{condlogit}.
#'
#' An object of class '\code{condlogit}' is a list with at least the following components:
#' \describe{
#'   \item{coef}{An \code{estimate} object with the target parameter estimates (see \code{\link[lava]{estimate.default}})}
#'   \item{vcov}{An \code{estimate} object with the target parameter estimates (see \code{\link[lava]{estimate.default}})}
#'   \item{opt}{Object returned from the applied optimization routine}
#'   \item{idx}{number of parameters of the model (target and nuisance)}
#'   \item{alternatives}{String describing the model}
#' \item{alternatives}{String describing the model}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object of class \code{targeted}:
#' \itemize{
#'   \item{\code{coef}}{Extract estimated model coefficients.}
#'   \item{\code{vcov}}{Extract the variance-covariance matrix of the parameters.}
#'   \item{\code{iid}}{Extract the estimated influence function.}
#'   \item{\code{print}}{Print parameter estimates.}
#'   \item{\code{summary}}{Extract  model.}'
#'  }
#'
#' @aliases condlogit-class
#' @return objects of the S3 class '\code{condlogit}'
#' @examples ## See example(condlogit) for examples
#' @docType class
#' @name condlogit-class
NULL


################################################################################

##' @export
print.condlogit <- function(x, ...) {
    print(x$call)
    cat("log-likelihood: ", -x$val, "\n\n")
    print(x$coef)
}

##' @export
summary.condlogit <- function(object,...) {
    lava::estimate(coef=object$coef, vcov=object$vcov, ...)
}

##' @export
logLik.condlogit <- function(object, ...) {
    n <- object$n
    structure(object$val,
              df=length(coef(object)),
              nobs=n,
              nall=n,
              class="logLik")
}

##' @export
coef.condlogit <- function(object, ...) {
    object$coef
}

##' @export
vcov.condlogit <- function(object, ...) {
    object$vcov
}

##' @export
predict.condlogit <- function(object, newdata, x=NULL, z1=NULL, z2=NULL, id=NULL, alt=NULL, wide=FALSE, ...) {
    ref <- with(object, alternatives[reference])
    browser()
    if (!is.null(object$formula)) {
        env <- with(object$formula,
                    condlogit_prep(choice, x=x, z1=z1, z2=z2,
                                   alt=alt, id=id, data=newdata))
    } else {
        if (is.null(alt)) alt <- rep(object$alternatives[1],nrow(x))
        levels <- intersect(object$alternatives, unique(alt))
        env <- condlogit_prep(x=x, z1=z1, z2=z2, alt=alt, id=id, alt_levels=levels)
    }
    theta <- coef(object)
    pr <- .mlogit_pred(theta, alt=env$alternatives-1,
                       basealt=object$reference-1,
                       nalt=length(env$levels),
                       id_idx=env$cluster$idx, z1=env$z1, z2=env$z2, x=env$x)
    browser()
    res <- data.frame(id=env$id,
                      alt=as.factor(object$alternatives[env$alt0+1]),
                      pred=pr[,1])
    if (!wide) return(res)
    res <- subset(mets::fast.reshape(res, id="id", num="alt", sep="."), select=-id)
    names(res) <- gsub("^pred\\.", "", names(res))
    res
}

################################################################################

condlogit_prep <- function(choice=NULL,
                    x=~1,   # individual specific covariates
                    z1=~0, 	# alternative-specific with constant coef.
                    z2=~0, 	# alternative-specific with varying coef.
                    alt=NULL,
                    id=NULL,
                    weights=NULL,
                    data=parent.frame(),
                    alt_levels, ## Optional factor levels of 'alt'
                    order=TRUE) {

    if (inherits(choice, "formula")) {
        xy <- lava::getoutcome(choice, sep="|")
        if (length(attr(xy,"x"))<2) {
            xx <- as.formula(paste0("~", as.character(choice[3])))
            xy <- structure(as.character(choice[2]), x=list(xx))
        }
        choice <- xy[1]
        if (!is.null(attr(xy,"x"))) {
            xx <- attr(xy,"x")
            if (length(xx)==1) {
                alt <- choice
                x <- model.matrix(xx[[1]], data=data)
            } else {
                if (is.null(alt))
                    alt <- model.frame(xx[[1]], data=data)[,1]
                if (length(attr(xy,"x"))>1 && is.null(id))
                    id <- model.frame(xx[[2]], data=data)[,1]
            }
        }
    }
    variables <- list()
    if (length(id)==1) {
        variables <- c(variables, list(id=id))
        id <- data[,id]
    }
    if (length(weights)==1) {
        variables <- c(variables, list(weights=weights))
        weights <- data[,weights]
    }
    if (length(choice==1)) {
        variables <- c(variables, list(choice=choice))
        if (choice%in%colnames(data))
            choice <- data[,choice]
    }
    if (length(alt)==1) {
        variables <- c(variables, list(alt=alt))
        alt <- data[,alt]
    }
    alt <- as.factor(alt)
    if (!missing(alt_levels)) {
        old <- levels(alt)
        levels(alt) <- c(alt_levels, setdiff(old, alt_levels))
    }
    n <- length(alt)
    alts <- levels(alt)
    J <- length(alts)
    if (is.null(x)) x <- matrix(nrow=n, ncol=0)
    if (is.null(z1)) z1 <- matrix(nrow=n, ncol=0)
    if (is.null(z2)) z2 <- matrix(nrow=n, ncol=0)
    if (!is.matrix(x)) {
        x <- model.matrix(x, data=data)
    }
    if (!is.matrix(z1)) {
        z1 <- model.matrix(update(z1,~.-1), data=data)
    }
    if (!is.matrix(z2)) {
        z2 <- model.matrix(update(z2,~.-1), data=data)
    }
    if (length(id)==0) id <- seq(length(alt))
    if (length(weights)==0) weights <- rep(1,length(alt))

    cl <- .clusterid(id)
    if (cl$n == n) { ## Data in short form ('standard' multinomial)
        id <- rep(1:n, each=J)
        newd <- .mlogit_expand(alt, x, weights, seq_along(alts))
        order <- FALSE
        alt <- newd$alt[,1]
        namX <- colnames(x)
        x <- newd$x
        colnames(x) <- namX
        choice <- newd$choice[,1,drop=TRUE]
        weights <- newd$weights[,1,drop=TRUE]
        cl$idx <- newd$id_idx[,1,drop=TRUE]
        cl$size <- rep(J,cl$n)
        rm(newd)
    }
    ord <- NULL
    if (order) {
        ord <- base::order(id)
        id <- rep(1:cl$n, each=J)
        choice <- choice[order]
        alt <- alt[order]
        weights <- weights[order]
        x <- x[order,,drop=FALSE]
        z1 <- z1[order,,drop=FALSE]
        z2 <- z2[order,,drop=FALSE]
    }

    par.idx <- list(z1=seq_len(NCOL(z1)),
                    z2=seq_len(J*NCOL(z2))+NCOL(z1),
                    x=seq_len((J-1)*NCOL(x))+NCOL(z1)+J*NCOL(z2),
                    J = J, n = n)
    res <- list(cluster=cl,
                choice=choice,
                order=ord,
                x=x,
                z1=z1,
                z2=z2,
                par.idx=par.idx,
                id=id,
                weights=weights,
                alternatives=alt,
                levels=alts)
    ##list2env(res)
    return(res)
}
