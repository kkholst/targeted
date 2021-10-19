##'  content for description
##'
##' content for details
##' @title Multiclass scoring
##' @param prob prob
##' @param obs obs
##' @param weights weights
##' @param metrics metrics
##' @param messages Adjust amount of warnings/messages (0 default: none)
##' @aliases Scoring ScoringMany
##' @export
##' @examples
##' data(iris)
##' set.seed(1)
##' dat <- csplit(iris,2)
##' g1 <- NB(Species ~ Sepal.Width + Petal.Length, data=dat[[1]])
##' g2 <- NB(Species ~ Sepal.Width, data=dat[[1]])
##' pr1 <- predict(g1, newdata=dat[[2]], wide=TRUE)
##' pr2 <- predict(g2, newdata=dat[[2]], wide=TRUE)
##' table(colnames(pr1)[apply(pr1,1,which.max)], dat[[2]]$Species)
##' table(colnames(pr2)[apply(pr2,1,which.max)], dat[[2]]$Species)
##' Scoring(pr1, dat[[2]]$Species)
##' ScoringMany(dat[[2]]$Species, pr1=pr1,pr2=pr2)
Scoring <-
function(prob, obs, weights = NULL, metrics = c("brier"), messages=0) {
    cl <- colnames(prob) # classes for which predictions are available
    cl.obs <- unique(obs)
    newcl <- which(!cl.obs%in%cl) # observed classes for which no predictions are available
    ## assigning pred 0 probabilities to unobserved classes
    if (length(newcl)) {
        if (messages>0) warning("new classes among observations")
        temp <- array(0, dim = c(nrow(prob), length(newcl)))
        colnames(temp) <- cl.obs[newcl]
        prob <- cbind(prob, temp)
    }
    y <- outer(obs, colnames(prob), "==")
    ## Brier score
    Bi <- apply((prob - y)^2, 1, sum)
    ## logscore
    li <- apply(log(prob) * y, 1, function(x) sum(x[is.finite(x)], na.rm=TRUE))
    if (!is.null(weights)) {
        B <- stats::weighted.mean(Bi, w = weights, na.rm = TRUE)
        L <- stats::weighted.mean(li, w = weights, na.rm = TRUE)
    } else {
        B <- mean(Bi, na.rm = TRUE)
        L <- mean(li, na.rm = TRUE)
    }
    return(list(brier = B, logscore = L))
}


##' @export
ScoringMany <- function(class, ..., weights=NULL, names=NULL) {
    S <- suppressWarnings(lapply(list(...), Scoring, obs=class, weights=weights))
    if (is.null(names)) {
        names <- base::names(list(...))
    }
    res <- matrix(unlist(S), byrow=TRUE, ncol=length(S[[1]]))
    rownames(res) <- names
    colnames(res) <- names(S[[1]])
    return(res)
}
