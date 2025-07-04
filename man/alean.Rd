% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alean.R
\name{alean}
\alias{alean}
\title{Assumption Lean inference for generalized linear model parameters}
\usage{
alean(
  response_model,
  exposure_model,
  data,
  link = "identity",
  g_model,
  nfolds = 1,
  silent = FALSE,
  mc.cores,
  ...
)
}
\arguments{
\item{response_model}{formula or \link{learner} object (formula => glm)}

\item{exposure_model}{model for the exposure}

\item{data}{data.frame}

\item{link}{Link function (g)}

\item{g_model}{Model for \eqn{E[g(Y|A,W)|W]}}

\item{nfolds}{Number of folds}

\item{silent}{supress all messages and progressbars}

\item{mc.cores}{mc.cores Optional number of cores.
parallel::mcmapply used instead of future}

\item{...}{additional arguments to future.apply::future_mapply}
}
\value{
alean.targeted object
}
\description{
Assumption lean inference via cross-fitting (Double ML). See
<doi:10.1111/rssb.12504
}
\details{
Let \eqn{Y} be the response variable, \eqn{A} the exposure and \eqn{W}
covariates. The target parameter is: \deqn{\Psi(P) = \frac{E(Cov[A,
g\{E(Y|A,W)\}\mid W])} {E\{Var(A\mid W)\}} }

The \code{response_model} is the model for \eqn{E(Y|A,W)}, and
\code{exposure_model} is the model for \eqn{E(A|W)}.
\code{link} specifies \eqn{g}.
}
\examples{

sim1 <- function(n, family=gaussian(), ...) {
   m <- lava::lvm() |>
     lava::distribution(~y, value=lava::binomial.lvm()) |>
     lava::regression('a', value=function(l) l) |>
     lava::regression('y', value=function(a,l) a + l)
     if (family$family=="binomial")
        lava::distribution(m, ~a) <- lava::binomial.lvm()
   lava::sim(m, n)
}

library(splines)
f <- binomial()
d <- sim1(1e4, family=f)
e <- alean(
 response_model=learner_glm(y ~ a + bs(l, df=3), family=binomial),
 exposure_model=learner_glm(a ~ bs(l, df=3), family=f),
 data=d,
 link = "logit", mc.cores=1, nfolds=1
)
e

e <- alean(response_model=learner_glm(y ~ a + l, family=binomial),
           exposure_model=learner_glm(a ~ l),
           data=d,
           link = "logit", mc.cores=1, nfolds=1)
e
}
\author{
Klaus Kähler Holst
}
