% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_functions.R
\name{as_sum}
\alias{as_sum}
\title{as_sum}
\usage{
as_sum(acc, input)
}
\arguments{
\item{acc}{the accumulater, can be empty}

\item{input}{the input, can be empty}
}
\value{
A step function that yields a sum
}
\description{
A normal reduction step function that sums over all items.
It also supports arity 0 and 1.
}

