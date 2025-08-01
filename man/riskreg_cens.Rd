% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/riskreg_cens.R
\name{riskreg_cens}
\alias{riskreg_cens}
\title{Binary regression models with right censored outcomes}
\usage{
riskreg_cens(
  response,
  censoring,
  treatment = NULL,
  prediction = NULL,
  data,
  newdata,
  tau,
  type = "risk",
  M = 1,
  call.response = "phreg",
  args.response = list(),
  call.censoring = "phreg",
  args.censoring = list(),
  preprocess = NULL,
  efficient = TRUE,
  control = list(),
  ...
)
}
\arguments{
\item{response}{Response formula (e.g., Surv(time, event) ~ D + W).}

\item{censoring}{Censoring formula (e.g., Surv(time, event == 0) ~ D + A +
W)).}

\item{treatment}{Optional treatment model (\link{learner})}

\item{prediction}{Optional prediction model (\link{learner})}

\item{data}{data.frame.}

\item{newdata}{Optional data.frame. In this case the uncentered influence
function evaluated in 'newdata' is returned with nuisance parameters
obtained from 'data'.}

\item{tau}{Time-point of interest, see Details.}

\item{type}{"risk", "treatment", "brier"}

\item{M}{Number of folds in cross-fitting (M=1 is no cross-fitting).}

\item{call.response}{Model call for the response model (e.g. "mets::phreg").}

\item{args.response}{Additional arguments to the response model.}

\item{call.censoring}{Similar to call.response.}

\item{args.censoring}{Similar to args.response.}

\item{preprocess}{(optional) Data pre-processing function.}

\item{efficient}{If FALSE an IPCW estimator is returned}

\item{control}{See details}

\item{...}{Additional arguments to lower level data pre-processing
functions.}
}
\value{
estimate object
}
\description{
Binary regression models with right censored outcomes
}
\details{
The one-step estimator depends on the calculation of an integral
wrt. the martingale process corresponding to the counting process N(t) =
I(C>min(T,tau)). This can be decomposed into an integral wrt the counting
process, \eqn{dN_c(t)} and the compensator \eqn{d\Lambda_c(t)} where the
latter term can be computational intensive to calculate. Rather than
calculating this integral in all observed time points, we can make a
coarser evaluation which can be controlled by setting
\code{control=(sample=N)}. With \code{N=0} the (computational intensive)
standard evaluation is used.
}
\author{
Klaus K. Holst, Andreas Nordland
}
