% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asreml.nvc.R
\name{asreml.nvc}
\alias{asreml.nvc}
\title{Call asreml allowing for negative variance components}
\usage{
asreml.nvc(...)
}
\arguments{
\item{...}{All arguments that are to be passed to \code{asreml}.
An optional vector \code{ginits} with initial values for
\code{gamma} (units: scaled dispersion) can be provided, with
values in the order in which the variance components are shown
in the output of asreml. If only some variances components
should be unbound, then the constraints can be specified in the
optional vector \code{gconstraints}.}
}
\value{
An asreml object as would be returned if \code{asreml} was called directly
}
\description{
ASReml can fit 'negative variance components', i.e. allow for
between subjects variances that are lower than would be expected based on the
variances of the components that are aggregated in these subjects.
This can occur by chance, or because of a systematic negative residual correlation
within the groups.
}
\details{
\code{asreml.nvc} essentially calls \code{asreml}, setting constraints to variance components to 'U'
by modifying \code{gammas.table} in the \code{G.param} argument.
}
\examples{
\dontrun{
library(pascal)
library(asreml)
data(nvc.example)
nvc.example.asr <- asreml(y~plotgroup*subplotgroup,random=~plot,data=nvc.example)
test.asreml(nvc.example.asr)
## ---- Wald tests:
##                        Df denDF F.inc     Pr
## (Intercept)             1    96 84340 <2e-16 ***
## plotgroup               1    96     1   0.48
## subplotgroup            1    96     0   0.50
## plotgroup:subplotgroup  1    96     0   0.92
##
## ---- Stratum variances:
##            df Variance R!variance
## R!variance 96    11.85          1
##
## ---- Variance components:
##                   gamma component std.error z.ratio constraint
## plot!plot.var 1.012e-07 1.199e-06 1.731e-07   6.928   Boundary
## R!variance    1.000e+00 1.185e+01 1.710e+00   6.928   Positive
##
## ---- Dispersion:
## 3.442

nvc.example.asr <- asreml.nvc(y~plotgroup*subplotgroup,random=~plot,data=nvc.example)
## ---- Wald tests:
##                        Df denDF  F.inc     Pr
## (Intercept)             1    48 152200 <2e-16 ***
## plotgroup               1    48      1   0.35
## subplotgroup            1    48      0   0.57
## plotgroup:subplotgroup  1    48      0   0.93
##
## ---- Stratum variances:
##            df Variance plot R!variance
## plot       48    6.568    2          1
## R!variance 48   17.133    0          1
##
## ---- Variance components:
##                 gamma component std.error z.ratio    constraint
## plot!plot.var -0.3083    -5.282     1.873  -2.821 Unconstrained
## R!variance     1.0000    17.133     3.497   4.899      Positive
##
## ---- Dispersion:
## 4.139
}
}
\author{
Pascal Niklaus \email{pascal.niklaus@ieu.uzh.ch}
}
