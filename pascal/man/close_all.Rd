% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/close_all.R
\name{close_all}
\alias{close_all}
\title{Close all text sinks and graphics devices}
\usage{
close_all()
}
\value{
none
}
\description{
Close sinks and graphics devices if any are open, issuing a warning.
This function is useful when a script fails between opening and closing
of a sink, or when these are not balanced.
}
\examples{
library(pascal)
sink("some_file.txt")
pdf("test.pdf")
close_all()
unlink("test.pdf");
unlink("some_file.txt");
}
\author{
Pascal Niklaus \email{pascal.niklaus@ieu.uzh.ch}
}
