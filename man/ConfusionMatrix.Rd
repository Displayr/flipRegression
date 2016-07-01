% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regressiondiagnostics.R
\name{ConfusionMatrix}
\alias{ConfusionMatrix}
\title{\code{ConfusionMatrix}}
\usage{
ConfusionMatrix(obj, subset = NULL, weights = NULL)
}
\arguments{
\item{obj}{A model with an outcome variable.}

\item{subset}{An optional vector specifying a subset of observations to be
used in the fitting process, or, the name of a variable in \code{data}. It
may not be an expression. \code{subset} may not}

\item{weights}{An optional vector of sampling weights, or, the name or, the
name of a variable in \code{data}. It may not be an expression.}
}
\description{
\code{ConfusionMatrix}
}
\details{
The proportion of observed values that take the same values as the predicted values.
Where the outcome
variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
value.
}
