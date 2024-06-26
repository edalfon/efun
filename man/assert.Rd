% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{assert}
\alias{assert}
\title{Assert a condition within a data frame}
\usage{
assert(.data, ..., msg = "Assertion does not hold", na.rm = TRUE)
}
\arguments{
\item{.data}{A data frame containing the variables used in the conditions.}

\item{...}{One or more expressions to be evaluated within the data frame.}

\item{msg}{A character vector specifying the message to include in the error
message if the assertion fails.}

\item{na.rm}{passed to \code{all()}: logical. If true NA values are removed
before the result is computed.}
}
\value{
The data frame \code{.data} if all conditions are met. Otherwise, an error
is thrown.
}
\description{
The \code{assert} function evaluates one or more conditions within the context
of a data frame. If all conditions hold true (evaluate to \code{TRUE}), the data
frame is returned (so as to seemingly work in dplyr pipelines).
Otherwise, an informative error message is printed and
the function stops execution. When running interactively, the function
opens a View on the data.frame with a column evaluating assertion conditions
(to make it easy interactively exploring what went wrong). If not
interactively, it prints the rows that fail (resorting to tibble's print
method).
}
\details{
This is an old function, just adapted to make it work with latests versions
of pckgs. In the meantime we have used several different approaches from
other packages. From the good-old stopifnot, to packages such as assertthat,
assertive, assertr, the great testthat, among others I cannot remember now.
Yet, we keep going back to this simple yet convinient approach. So let's
put it here in efun, 'cause anyway, which we pull a few functions
from here again and again in different projects.
}
