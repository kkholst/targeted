
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
##' @param basealt Base alternative (reference)
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
##' and the data.frame only contains the rows where `choice==1` (similar to for example nnet::multinom).
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
               basealt=1,
               control=list(),
               method="nlminb",
               lambda2=0,
               debug=FALSE, ...) {
    ## Set-up
    cl <- match.call()
    env <- condlogit_prep(choice, x=x, z1=z1, z2=z2, alt=alt, id=id, weights=weights, data=data, basealt=basealt)
    idx <- env$par.idx
    obj <- function(theta,...) condlogit_obj(theta,env=env,...)
    f2 <- function(theta,...) -condlogit_loglik(theta,env=env,...)
    g2 <- function(theta,...) -obj(theta,...)$grad
    h2 <- function(theta,...) -obj(theta,return_hessian=TRUE,...)$hess
    theta0 <- control$start
    control
    if (is.null(theta0)) {
        ## Starting values
        pX <- with(env$par.idx, rep(0.1,length(x)))
        pZ1 <- with(env$par.idx, rep(0.1,length(z1)))
        pZ2 <- with(env$par.idx, rep(0.1,length(z2)))
        theta0 <- c(pZ1, pZ2, pX)
    }
    if (debug) {
        ## print(numDeriv::jacobian(f2,theta0))
        ## print(g2(theta0))
        ## print(numDeriv::jacobian(g2,theta0))
        ## print(h2(theta0))
        browser()
        env0 <- env; env0$weights <- rep(1,length(env0$weights))
        S <- condlogit_obj(theta0,env=env0,return_hessian=TRUE)$grad
        Sw <- apply(S,2,function(x) x*weights)
    }
    
    ## Optimization
    if (tolower(method)=="nr") {
        myobj <- function(theta,...) {
            val <- obj(theta,return_hessian=TRUE,...)
            res <- -val$hess
            attr(res, "grad") <- -colSums(val$grad)
            res
        }
        op <- lava::NR(hessian=myobj, theta0, f2)
    } else {
        op <- stats::nlminb(theta0, f2, g2, h2, control=control)
        ##op <- stats::nlminb(theta0, f2, g2, control=control)            
    }
    theta <- op$par
    alts <- env$alternatives
    alts0 <- alts[-basealt]
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
    val <- obj(theta,return_hessian=TRUE)
    vc <- -lava::Inverse(val$hess)
    dimnames(vc) <- list(nam,nam)

    formulas <- NULL
    if (all(unlist(lapply(list(x,z1,z2),function(x) inherits(x,"formula"))))) {
        formulas <- list(choice=choice, x=x, z1=z1, z2=z2)
    }
    res <- list(call=cl, opt=op, coef=theta, vcov=vc, idx=idx, val=obj(theta),
                basealt=basealt, alternatives=env$alternatives,
                variables=env$variables, n=env$cl$n, nrow=length(env$choice),
                formula=formulas, order=env$ord)
    return(structure(res, class="condlogit"))
}


################################################################################

##' @export
print.condlogit <- function(x, ...) {
    print(x$call)
    cat("log-likelihood: ", -x$val$ll, "\n\n")
    print(x$coef)    
}

##' @export
summary.condlogit <- function(object,...) {
    lava::estimate(coef=object$coef, vcov=object$vcov, ...)
}

##' @export
logLik.condlogit <- function(object, ...) {
    n <- object$n
    structure(object$val$ll,
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
    if (!is.null(object$formula)) {
        env <- with(object$formula,
                    condlogit_prep(choice, x=x, z1=z1, z2=z2,
                                   alt_levels=object$alternatives, alt=alt, id=id, data=newdata, basealt=object$basealt))
    } else {
        if (is.null(alt)) alt <- rep(object$alternatives[1],nrow(x))
        env <- condlogit_prep(x=x, z1=z1, z2=z2, alt=alt, id=id, alt_levels=object$alternatives)
    }
    theta <- coef(object)
    pr <- .mlogit_pred(theta, alt=env$alt0, basealt=object$basealt-1, nalt=length(env$alt0.idx), 
                       id_idx=env$cluster$idx, z1=env$z1, z2=env$z2, x=env$x)
    res <- data.frame(id=env$id, alt=as.factor(object$alternatives[env$alt0+1]), pred=pr[,1])
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
                 basealt=1,
                 alt_levels, ## Optional factor levels of 'alt'
                 order=TRUE
                 ) {

    if (inherits(choice, "formula")) {
        xy <- lava::getoutcome(choice, sep="|")
        if (length(attr(xy,"x"))<2) {
            xx <- as.formula(paste0("~", as.character(choice[3])))
            xy <- structure(as.character(choice[2]),x=list(xx))
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
        choice <- newd$choice[,1]
        weights <- newd$weights[,1]
        cl$idx <- newd$id_idx[,1]
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

    alt.idx = setdiff(seq(J), basealt)
    alt0.idx <- numeric(J); alt0.idx[alt.idx] <- seq(J-1)
    alt0 <- as.numeric(alt)
    par.idx <- list(z1=seq_len(NCOL(z1)),
                    z2=seq_len(J*NCOL(z2))+NCOL(z1),
                    x=seq_len((J-1)*NCOL(x))+NCOL(z1)+J*NCOL(z2),
                    alt.idx = alt.idx,
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
                alt0=alt0-1,
                alt0.idx=alt0.idx,
                alternatives=alts,
                basealt=basealt-1,
                variables=variables)                
    ##list2env(res)
    return(res)
}

condlogit_obj <- function(theta, env, ...)
    .mlogit(theta=theta,
            choice=env$choice,
            alt=env$alt0,
            basealt=env$basealt,
            nalt=length(env$alt0.idx),
            id_idx=env$cluster$idx,
            z1=env$z1,
            z2=env$z2,
            x=env$x,
            weights=env$weights, ...)

condlogit_loglik <- function(theta, env, ...)
    .mlogit_loglik(theta=theta,
            choice=env$choice,
            alt=env$alt0,
            basealt=env$basealt,
            nalt=length(env$alt0.idx),
            id_idx=env$cluster$idx,
            z1=env$z1,
            z2=env$z2,
            x=env$x,
            weights=env$weights, ...)
