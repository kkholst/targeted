% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RATE.R
\name{RATE.surv}
\alias{RATE.surv}
\title{Responder Average Treatment Effect}
\usage{
RATE.surv(
  response,
  post.treatment,
  treatment,
  censoring,
  tau,
  data,
  M = 5,
  pr.treatment,
  call.response,
  args.response = list(),
  SL.args.post.treatment = list(family = binomial(), SL.library = c("SL.mean", "SL.glm")),
  call.censoring,
  args.censoring = list(),
  preprocess = NULL,
  ...
)
}
\arguments{
\item{response}{Response formula (e.g., Surv(time, event) ~ D + W).}

\item{post.treatment}{Post treatment marker formula (e.g., D ~ W)}

\item{treatment}{Treatment formula (e.g, A ~ 1)}

\item{censoring}{Censoring formula (e.g., Surv(time, event == 0) ~ D + A +
W)).}

\item{tau}{Time-point of interest, see Details.}

\item{data}{data.frame}

\item{M}{Number of folds in cross-fitting (M=1 is no cross-fitting)}

\item{pr.treatment}{(optional) Randomization probability of treatment.}

\item{call.response}{Model call for the response model (e.g. "mets::phreg").}

\item{args.response}{Additional arguments to the response model.}

\item{SL.args.post.treatment}{Arguments to SuperLearner for the post
treatment indicator}

\item{call.censoring}{Similar to call.response.}

\item{args.censoring}{Similar to args.response.}

\item{preprocess}{(optional) Data preprocessing function}

\item{...}{Additional arguments to lower level functions}
}
\value{
estimate object
}
\description{
Estimation of the Average Treatment Effect among Responders for Survival
Outcomes
}
\details{
Estimation of
\deqn{
\frac{P(T \leq \tau|A=1) - P(T \leq \tau|A=1)}{E[D|A=1]}
}
under right censoring based on plug-in estimates of \eqn{P(T \leq \tau|A=a)}
and \eqn{E[D|A=1]}.

An efficient one-step estimator of \eqn{P(T \leq \tau|A=a)} is constructed
using the efficient influence function
\deqn{
\frac{I\{A=a\}}{P(A = a)} \Big(\frac{\Delta}{S^c_{0}(\tilde T|X)}
I\{\tilde T \leq \tau\} + \int_0^\tau
\frac{S_0(u|X)-S_0(\tau|X)}{S_0(u|X)S^c_0(u|X)} d M^c_0(u|X)\Big)
}
\deqn{
+ \Big(1 - \frac{I\{A=a\}}{P(A = a)}\Big)F_0(\tau|A=a, W)
- P(T \leq \tau|A=a).
}
An efficient one-step estimator of \eqn{E[D|A=1]} is constructed using the
efficient influence function
\deqn{
\frac{A}{P(A = 1)}\left(D-E[D|A=1, W]\right) + E[D|A=1, W] -E[D|A=1].
}
}
\author{
Andreas Nordland, Klaus K. Holst
}
