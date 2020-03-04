subset_t <- function(t, n.qt=250, n.eq=250) {    
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
    return(idx1)
}


gof_samplestat <- function(cumres_obj, R=1e3, x=NULL, idx=seq_along(x)) {
    if (!is.null(x)) cumres_obj$reorder(x)
    teststat <- cumres_obj$samplestat(R, idx-1, FALSE)
    W0 <- cumres_obj$obs()[idx]
    t0 <- cumres_obj$t[idx]
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
      R=1000, b=0, plots=min(R,50),
      coef=stats::coef(model), ic=lava::iid(model), breakties=1e-12,
      subset=subset_t,
      subset.max=250,
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
        variable <- as.data.frame(variable)
        for (x in variable) {
            idx <- seq_along(x)            
            if (!is.null(subset) && !is.null(subset.max)) idx <- subset(x,subset.max)
            cr$reorder(x+untie)
            teststat <- gof_samplestat(cr, R=R, idx=idx)
            KS <- c(KS, teststat$pval.ks)
            CvM <- c(CvM, teststat$pval.cvm)
            UsedData <- c(UsedData, list(teststat$t))
            W <- c(W, list(teststat$w))
            What0 <- matrix(0, length(idx), plots)
            for (i in seq(plots)) What0[,i] <- cr$sample1(idx-1)
            What <- c(What, list(What0))
            mytype <- c(mytype,"residual")
        }     
    
        res <- list(W=W, What=What,
                    x=UsedData,
                    KS=KS, CvM=CvM,
                    R=R, n=n,
                    sd=NULL, cvalues=NULL,
                    variable=names(variable),
                    type=mytype, untie=untie,
                    model=class(model)[1])
        class(res) <- "cumres"        
        return(res)    
    }
