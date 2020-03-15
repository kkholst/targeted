subset_t <- function(t, n.qt=250L, n.eq=250L) {
    if (length(t)<= n.qt+n.eq) return(seq_along(t))
    t <- sort(t)
    t1a <- t[mets::fast.approx(t, seq(t[1], t[length(t)], length.out=n.eq))]
    t1b <- quantile(t, seq(0,1,length.out=n.qt))
    t1 <- sort(union(t1a, t1b))
    pos <- 1
    idx <- numeric(length(t)) # grouping
    for (i in seq_along(idx)) {
        if (t[i]>t1[pos]) {
            pos <- pos+1        
        }
        idx[i] <- pos
    }
    idx1 <- mets::dby(data.frame(id=idx, pos=seq_along(idx)), pos~id, pos=max, REDUCE=T)$pos
    if (length(idx1)<n.qt+n.eq) {
        ## Add extra random points to fill up
        idx1 <- c(idx1, sample(setdiff(seq_along(t), idx1), (n.qt+n.eq)-length(idx1), replace=FALSE))
    }
    idx1 <- sort(idx1)
    return(idx1)
}


gof_samplestat <- function(cumres_obj, R=1e3L, x=NULL, idx=seq(NROW(x))) {
    if (!is.null(x)) cumres_obj$order(x)
    teststat <- cumres_obj$samplestat(R, cbind(idx-1), FALSE)
    t0 <- cumres_obj$inp[idx,1]    
    W0 <- cumres_obj$obs(0)[idx]
    ks0 <- SupTest(W0)
    cvm0 <- L2Test(W0,t0)
    return(list(pval.ks=mean(teststat[,1]>ks0), ks=ks0,
                pval.cvm=mean(teststat[,2]>cvm0), cvm=cvm0,
                t=t0, w=W0))
}


##' @export
`cumres` <-
function(model,...) UseMethod("cumres")

##' @export
`cumres.default` <-
    function(model, variable, r=stats::residuals(model), dr,  
      R=1e3L, b=0, plots=min(R,50L),
      coef=stats::coef(model), ic=lava::iid(model), breakties=1e-12,
      subset=subset_t,
      subset.max=500L,
      ...) {

        if(any(is.na(coef))) stop("Over-parametrized model")
        if (missing(dr)) {            
            dr <- attributes(r)$grad    
            if (is.null(dr)) {
                if (!requireNamespace("numDeriv")) stop("Supply gradient")
                dr <- numDeriv::jacobian(r, coef, ...)
            }
        } else {
            if (is.function(dr))
                dr <- dr(coef)
        }
        if (is.function(r)) {
            r <- r(coef,...)
        }
                
        n <- length(r)
        untie <- runif(n,0,breakties)
        cr <- new(CumRes, r, dr, ic)         
        W <- c()
        What <- c()
        KS <- c()
        CvM <- c()
        mytype <- c()
        UsedData <- c()
        vnames <- names(as.data.frame(variable))
        variable <- apply(cbind(as.matrix(variable)),2,function(x) x+untie)
        p <- NCOL(variable)
        
        Idx <- matrix(ncol=0, nrow=0) 
        if (!is.null(subset.max) && n>subset.max) {
            ns <- ceiling(subset.max/2)
            Idx <- matrix(0, 2*ns, p)
            for (i in seq(p)) {
                idx <- subset_t(variable[,i,drop=TRUE],
                                n.qt=ns, n.eq=ns)
                Idx[,i] <- idx-1
            }
        }

        cr$order(variable)
        teststat <- cr$samplestat(R, Idx, FALSE)

        W0 <- cr$obs()
        W <- lapply(seq(ncol(W0)), function(x) W0[,x])

        for (i in seq(p)) {
            t0 <- cr$inp[,i]
            if (length(Idx)>0) {
                t0 <- t0[Idx[,i]]
                W[[i]] <- W[[i]][Idx[,i]]
            }
            ks0 <- SupTest(W[[i]])
            cvm0 <- L2Test(W[[i]],t0)
            pval.ks <- mean(teststat[,2*(i-1)+1]>ks0)
            pval.cvm <- mean(teststat[,2*(i-1)+2]>cvm0)
            KS <- c(KS, pval.ks)
            CvM <- c(CvM, pval.cvm)
            UsedData <- c(UsedData, list(t0))
            What0 <- matrix(0, length(W[[i]]), plots)
            What <- c(What, list(What0))
            mytype <- c(mytype,"residual")            
        }
        for (j in seq(plots)) {
            sim <- cr$sample1(Idx)
            for (i in seq(p))
                What[[i]][,j] <- sim[,i]
        }
           
        res <- list(W=W, What=What,
                    x=UsedData,
                    KS=KS, CvM=CvM,
                    R=R, n=n,
                    sd=NULL, cvalues=NULL,
                    variable=vnames,
                    type=mytype, untie=untie,
                    model=class(model)[1])
        class(res) <- "cumres"        
        return(res)    
    }
