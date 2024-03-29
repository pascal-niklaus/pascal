% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel.hist.R
\name{panel.line}
\alias{panel.line}
\title{panel with regression line for \code{pairs}}
\usage{
panel.line(
  x,
  y,
  type = "OLS",
  col = rgb(1, 0, 0, alpha = 0.5),
  pch = 16,
  cex = 1,
  line.col = "blue",
  line.lwd = 2,
  line.lty = c(1, 2),
  ...
)
}
\arguments{
\item{x}{values passed from \code{\link{pairs}}}

\item{y}{values passed from \code{\link{pairs}}}

\item{type}{the type of regression to be performed. Default is
\code{'OLS'}. See \code{lmodel2} for other options.}

\item{col, pch, cex}{symbol color, type, and size}

\item{line.col, line.lwd, line.lty}{regression line color, width, and
type. All arguments can be vectors of length two, in which case
the 2nd element is chosen when the regression slope is
statistically significant at P<0.05.}

\item{...}{additional arguments passed to \code{lmodel2}.}
}
\description{
\code{pairs} offers panel functions, but the one to display a
regression line in the off-diagonal elements is not predefined.
}
\details{
This function implements such a function, allowing to chose between
OLS regression (the default) and main axis regression. Internally,
it calls \code{lmodel2}.
}
\examples{
data(iris, package = "datasets")

pairs(iris[, 1:4],
    diag.panel = panel.hist,
    lower.panel = panel.line
)

pairs(iris[,1:4],
      diag.panel=panel.hist,
      lower.panel=function(x,y) {
          panel.line(x, y, range.y = "relative", range.x = "relative", type = "RMA",
                     col = "green", cex = 2,
                     line.col = c("red","gray"),
                     line.lwd = c(2, 1),
                     line.lty = c(1, 3))
      }
)

}
\seealso{
\code{\link{pairs}}, \code{lmodel2}
}
\author{
Pascal Niklaus \email{pascal.niklaus@ieu.uzh.ch}
}
