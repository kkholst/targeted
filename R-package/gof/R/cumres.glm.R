##' @export
`cumres.lm` <- function(model,...) {
  cumres.glm(model,...)
}

##' Calculates GoF statistics based on cumulative residual processes
##' 
##' Given the generalized linear models model \deqn{g(E(Y_i|X_{i1},...,X_{ik})) =
##' \sum_{i=1}^k \beta_jX_{ij}} the \code{cumres}-function calculates the the
##' observed cumulative sum of residual process, cumulating the residuals,
##' \eqn{e_i}, by the jth covariate: \deqn{W_j(t) = n^{-1/2}\sum_{i=1}^n
##' 1_{\{X_{ij}<t\}}e_i} and Sup and L2 test
##' statistics are calculated via simulation from the asymptotic distribution of
##' the cumulative residual process under the null (Lin et al., 2002).
##' 
##' 
##' @aliases cumres cumres.glm cumres.lm
##' @param model Model object (\code{lm} or \code{glm})
##' @param variable List of variable to order the residuals after
##' @param data data.frame used to fit model (complete cases)
##' @param R Number of samples used in simulation
##' @param b Moving average bandwidth (0 corresponds to infinity = standard
##' cumulated residuals)
##' @param plots Number of realizations to save for use in the plot-routine
##' @param ... additional arguments
##' @return Returns an object of class 'cumres'.
##' @note Currently linear (normal), logistic and poisson regression models with
##' canonical links are supported.
##' @author Klaus K. Holst
##' @seealso \code{\link[timereg]{cox.aalen}} in the \code{timereg}-package for
##' similar GoF-methods for survival-data.
##' @references D.Y. Lin and L.J. Wei and Z. Ying (2002) \emph{Model-Checking
##' Techniques Based on Cumulative Residuals}. Biometrics, Volume 58, pp 1-12.
##' 
##' John Q. Su and L.J. Wei (1991) \emph{A lack-of-fit test for the mean function
##' in a generalized linear model}. Journal. Amer. Statist. Assoc., Volume 86,
##' Number 414, pp 420-426.
##' @keywords models regression
##' @export
##' @examples
##' 
##' sim1 <- function(n=100, f=function(x1,x2) {10+x1+x2^2}, sd=1, seed=1) {
##'   if (!is.null(seed))
##'     set.seed(seed)
##'   x1 <- rnorm(n);
##'   x2 <- rnorm(n)
##'   X <- cbind(1,x1,x2)
##'   y <- f(x1,x2) + rnorm(n,sd=sd)
##'   d <- data.frame(y,x1,x2)
##'   return(d)
##' }
##' d <- sim1(100); l <- lm(y ~ x1 + x2,d)
##' system.time(g <- cumres(l, R=100, plots=50))
##' g
##' \donttest{plot(g)}
##' g1 <- cumres(l, c("y"), R=100, plots=50)
##' g1
##' g2 <- cumres(l, c("y"), R=100, plots=50, b=0.5)
##' g2
##' 
`cumres.glm` <- function(model,
                  variable=c("predicted",colnames(model.matrix(model))),
                  data=data.frame(model.matrix(model)), 
                  R=1000, b=0, plots=min(R,50),
                  ...) {
  dginv <- function(z) 1
  a.phi <- 1
  switch(class(model)[1],
         lm = {
           a.phi <- summary(model)$sigma^2
           h <- function(z) 1
         },
         glm = {
           f <- family(model)
           if (f$family=="gaussian") {
             a.phi <- summary(model)$dispersion
           }

           w <- model.extract(model.frame(model),"weights")
           if (!is.null(w)) {
               stop("Weight argument not supported")
           }
           if (grepl("matrix",attr(terms(model),"dataClasses")[1])) {
               ## success,failure -> refit
               y <- model.extract(model.frame(model),"response")               
               X <- model.matrix(model)
               y0 <- unlist(apply(y[,1:2],1,function(x) rep(c(1,0),as.vector(x))))
               X0 <- as.matrix(X[rep(seq(nrow(y)),rowSums(y[,1:2])),,drop=FALSE])
               cl <- model$call
               cl$formula <- y0 ~ -1+X0
               cl$data <- NULL
               model <- eval(cl)
           }                  
           g <- f$linkfun
           ginv <- f$linkinv
           dginv <- f$mu.eta ## D[linkinv]
           ##dg <- function(x) 1/dginv(g(x)) ## Dh^-1 = 1/(h'(h^-1(x)))
           canonf <- do.call(f$family,list())           
           caninvlink <- canonf$linkinv
           canlink <- canonf$linkfun
           Dcaninvlink <- canonf$mu.eta           
           Dcanlink <- function(x) 1/Dcaninvlink(canlink(x))
           ##gmu <- function(x) g(caninvlink(x))
           ##invgmu <- function(z) canlink(ginv(z))
           h <- function(z) Dcanlink(ginv(z))*dginv(z)                                
         },
         stop("Unsupported model!"))

  response <- all.vars(formula(model))[1]
  X <- model.matrix(model)
  n <- nrow(X)  
  r <- residuals(model, type="response") ## y-g^{-1}(Xb)
  yhat <- predict(model, type="response") ## g^{-1}(Xb)
  ## Xbeta <- predict(model, type="link") ## X*b
  beta <- coef(model)
  Xbeta <- X%*%beta
  if(any(is.na(beta))) stop("Over-parametrized model")
 
  dr <- -(as.numeric(dginv(Xbeta))*X)
  ic <- lava::iid(model)

  if (!is.na(match(response, variable))) variable[match(response, variable)] <- "predicted"
  variable <- unique(variable)
  UsedData <- X[,na.omit(match(variable, colnames(X))),drop=FALSE]
  myvars <- colnames(UsedData)[apply(UsedData,2,function(x) length(unique(x))>2)] ## Only consider variables with more than two levels
  if ("predicted"%in%variable) myvars <- c("predicted",myvars)  

  variable <- c()
  for (v in myvars) {
    x <- NULL
    if (v=="predicted") {
        x <- yhat
    } else  if (v %in% colnames(X)) {
        x <- X[,v]    
    } else warning("Variable '", v , "' not found.\n",sep="")
    if (!is.null(x)) {
        variable <- cbind(variable, x)
        colnames(variable)[ncol(variable)] <- v
    }
}
  
  res <- cumres.default(NULL, variable=variable,
                        r=r, dr=dr, ic=ic, coef=NULL,
                        R=R, plots=plots, b=b, ...)
  
  return(res)
}

