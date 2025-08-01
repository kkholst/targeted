% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learner_naivebayes.R
\name{learner_naivebayes}
\alias{learner_naivebayes}
\title{Construct a learner}
\usage{
learner_naivebayes(
  formula,
  info = "Naive Bayes",
  laplace.smooth = 0,
  kernel = FALSE,
  learner.args = NULL,
  ...
)
}
\arguments{
\item{formula}{(formula) Formula specifying response and design matrix.}

\item{info}{(character) Optional information to describe the instantiated
\link{learner} object.}

\item{laplace.smooth}{Laplace smoothing}

\item{kernel}{If TRUE a kernel estimator is used for numeric predictors
(otherwise a gaussian model is used)}

\item{learner.args}{(list) Additional arguments to
\link[=learner]{learner$new()}.}

\item{...}{Additional arguments to \link{naivebayes}.}
}
\value{
\link{learner} object.
}
\description{
Constructs a \link{learner} class object for fitting a naive bayes
classifier with \link{naivebayes}. As shown in the examples, the constructed
learner returns predicted class probabilities of class 2 in case of binary
classification. A \verb{n times p} matrix, with \code{n} being the number of
observations and \code{p} the number of classes, is returned for multi-class
classification.
}
\examples{
n <- 5e2
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n)
y <- rbinom(n, 1, lava::expit(x2*x1 + cos(x1)))
d <- data.frame(y, x1, x2)

# binary classification
lr <- learner_naivebayes(y ~ x1 + x2)
lr$estimate(d)
lr$predict(head(d))

# multi-class classification
lr <- learner_naivebayes(Species ~ .)
lr$estimate(iris)
lr$predict(head(iris))
}
