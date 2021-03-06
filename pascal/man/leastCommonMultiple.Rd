% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcmgcd.R
\name{leastCommonMultiple}
\alias{leastCommonMultiple}
\title{Least common multiple}
\usage{
leastCommonMultiple(a, b = NULL)
}
\arguments{
\item{a}{integer, or vector of integers in no parameter b is passed}

\item{b}{integer}
}
\value{
least common multiple
}
\description{
Given two integers, or a vector of integers, determine the least common multiple.
}
\details{
This function is not called \code{lcm} because there already is a function of that name
in \code{package:graphics}.
}
\examples{
library(pascal)
leastCommonMultiple(3,4)
leastCommonMultiple(c(3,4,5))
}
\references{
This functions are adapted from library \code{pracma}
}
\author{
Pascal Niklaus \email{pascal.niklaus@ieu.uzh.ch}
}
