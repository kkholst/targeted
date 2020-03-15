##' Plot cumulative residuals from a 'cumres' object
##'
##' \code{plot} displays the observed cumulative residual process with
##' realizations under the null.
##'
##'
##' @param x Object produced by the function \code{cumres}.
##' @param idx vector of numbers (or variable names) indicating which processes
##' from the \code{x} to plot.
##' @param col Color of the sample processes. By setting this parameter to "none"
##' or \code{NULL} no realizations will be drawn. The number of realizations is
##' determined by the \code{cumres}-object.
##' @param col.alpha Transparency-level of plotted sample processes
##' @param legend Type of legend where "type1" gives p-values of GoF-tests and
##' "type2" gives usual type of legends.
##' @param xlab Optional label of x-axis
##' @param ylab Optional label of y-axis
##' @param vs Label of predictor
##' @param ylim Range of y axis
##' @param title Main title
##' @param ... Additional arguments passed to the plot-routine.
##' @author Klaus K. Holst
##' @keywords hplot regression
##' @examples
##'
##' n <- 500; x <- abs(rnorm(n,sd=0.2))+0.01; y <- sqrt(x) + rnorm(n,sd=0.2)
##' l <- lm(y ~ x)
##' g <- cumres(l, R=500)
##' plot(g, idx=1, legend="type2")
##' @method plot cumres
##' @export
plot.cumres <- function(x, idx=seq_along(x$W),
                 col="purple",
                 col.alpha=0.3,
                 legend=c("type1","type2","none"), xlab, ylab,
                 vs=TRUE,
                 ylim=NULL,
                 title,
                 ...) {
    ylim. <- ylim
    newylab <- missing(ylab)
    newxlab <- missing(xlab)
    for (i in idx) {
        legendtxt <- c(); legendpch <- c(); legendcol <- c(); legendlty <- c(); legendlwd <- c(); legendcex <- c()
        if (is.null(ylim)) {
            ylim. <- max(abs(x$W[[i]]))*2*c(-1,1)
        }
        ## Observed process
        main <- ""
        if (newxlab) {
            xlab <- x$variable[i];
            if (x$type[i]=="score") {
                main <- xlab; xlab <- "Time";
            }
        }
        if (newylab) {
            ylab <- substitute(expression(W[p](x)),list(p=x$variable[i]))
        }
        legendtxt <- c(legendtxt, "Observed"); legendpch <- c(legendpch,-1); legendcol <- c(legendcol,1); legendlty <- c(legendlty,1); legendlwd <- c(legendlwd,2); legendcex <- c(legendcex,1);
        x0 <- x$x[[i]]
        if (!vs) {
            x0 <- 1:length(x0)
            xlab <- "Observation"
        }

        sampleproc <- function() {
            ## Sample processes
            if (col!="none" && !is.null(col)) {
                legendtxt <- c(legendtxt, "Sample process under null"); legendpch <- c(legendpch,-1); legendcol <- c(legendcol,col); legendlty <- c(legendlty,1); legendlwd <- c(legendlwd,1); legendcex <- c(legendcex,1);
                graphics::matplot(x0, x$What[[i]], type="s", col=lava::Col(col,col.alpha), lwd=1, add=TRUE, lty=1)
            }
        }
        with(x, plot(W[[i]] ~ x0, type="n", lwd=2, ylab=ylab, ylim=ylim.,xlab=xlab,main=main));
        sampleproc()
        with(x, lines(W[[i]] ~ x0, type="s", lwd=2))

        if (!missing(title)) {
            graphics::title(title)
        } else {
            if (!is.null(x$response))
                graphics::title(x$response)
        }

        if (!is.null(legend) && legend[1]!="none" && (legend[1]!=F)) {
            if (legend[1]=="type1") {
                ltxt <- NULL
                if (!is.null(x$KS)) ltxt <- c(ltxt,paste0("Sup-test p=",format(x$KS[i],digits=3)))
                if (!is.null(x$CvM)) ltxt <- c(ltxt,paste0("L2-test p=",format(x$CvM[i],digits=3)))
                legend("topright", ltxt, bg="white")
            }
            else
                legend("topright", legendtxt, lty=legendlty, pch=legendpch, col=legendcol, lwd=legendlwd, pt.cex=legendcex, bg="white")
        }
        ylim. <- NULL
    }
    invisible(x)
}



