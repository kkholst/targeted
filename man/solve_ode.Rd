% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ode.R
\name{solve_ode}
\alias{solve_ode}
\title{Solve ODE}
\usage{
solve_ode(ode_ptr, input, init, par = 0)
}
\arguments{
\item{ode_ptr}{pointer (externalptr) to C++ function or an R function}

\item{input}{Input matrix. 1st column specifies the time points}

\item{init}{Initial conditions}

\item{par}{Parameters defining the ODE (parsed to ode_ptr)}
}
\value{
Matrix with solution
}
\description{
Solve ODE with Runge-Kutta method (RK4)
}
\details{
The external point should be created with the function
\code{targeted::specify_ode}.
}
\examples{
example(specify_ode)
}
\seealso{
specify_ode
}
\author{
Klaus Kähler Holst
}
