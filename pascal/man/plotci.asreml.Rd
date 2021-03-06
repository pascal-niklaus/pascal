% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotci.asreml.R
\name{plotci.asreml}
\alias{plotci.asreml}
\title{Plot confidence intervals based on ASReml predictions}
\usage{
plotci.asreml(
  data,
  formula = NULL,
  subset = NULL,
  backfun = I,
  xfun = I,
  pts.col = NULL,
  pts.pch = 16,
  pts.cex = 1,
  fill.col = NULL,
  line.lty = 1,
  line.lwd = 1,
  line.col = NULL,
  ci.lty = 1,
  ci.lwd = 1,
  ci.col = NULL,
  ci.f = 1
)
}
\arguments{
\item{data}{Data frame containing the result of a call to
\code{predict.asreml}. For more flexibility, the elements
$predictions or $predictions$pvals can also be supplied.}

\item{formula}{Right-sided formula indicating the independent
variable to plot on the x axis (e.g. ~x).
If not specified, the first column is
assumed to contain this data.}

\item{subset}{Logical (condition) specifying which data to
use. This expression is evaluated in the context of the
data frame (data$predictions$pvals) and the caller's
environment. This is useful when looping over groups.
See example below for an application.}

\item{backfun}{Function applied to the data before plotting. This
is useful to back-transform data to the measurement scale.}

\item{xfun}{Transformation applied to the independent variable before
plotting. Useful if the plot has the x axis transformed.}

\item{pts.col}{Color of the symbols showing predicted values
(NULL = not plotted)}

\item{pts.pch}{Symbol for predicted values in plot}

\item{pts.cex}{Scaling (size) of predicted values in plot}

\item{fill.col}{Fill color for area between confidence intervals
(NULL = not plotted)}

\item{line.lty}{Type of line connecting predicted values}

\item{line.lwd}{Width of line connecting predicted values}

\item{line.col}{Color of line that connects predicted values
(NULL = not plotted)}

\item{ci.lty}{Line type for confidence intervals}

\item{ci.lwd}{Line width for confidence intervals}

\item{ci.col}{Color for line showing confidence intervals}

\item{ci.f}{Number of standard errors for confidence intervals (default 1)}
}
\description{
\code{plotci.asreml} adds predictions and confidence intervals to
a plot. It offers the possibility to back-transform the predictions
to the original scale by passing a user-defined function
(\code{backfun}). Similarly, the independent variable plotted on
the x axis can be transformed, if required (\code{xfun}).
}
\details{
It is possible to plot different elements of predictions. Whether these
are shown or not is controlled by the respective color (\code{pts.col},
\code{line.col}, \code{fill.col}, \code{ci.col}). \code{NULL} indicates
that the respective part is omitted.

For points, the symbol and size is controlled by \code{pts.pch} and
\code{pts.cex}. Line type and width can be controlled for the line
connecting the predictions and the confidence intervals
(\code{line.lty}, \code{line.lwd}, \code{ci.lty}, \code{line.lwd}).

By default, uncertainty estimates of one standard error are
plotted. This can be changed by modifying the factor \code{ci.f}
(e.g. to 2 for 95\% C.I.).

Before plotting, both x (\code{xfun}) and y (\code{backfun}) data
can be (back)transformed by providing user-defined function. This
is useful to show predictions on the original scale even if they
were analysed after transformation.

It is possible to specify a condition defining a subset. This
condition is evaluated in the context of the data.frame with the
predicted values (which is part of the object returned by
\code{asreml.predict}) and in the caller's frame. See the example
provided below for a demonstration of this feature.
}
\examples{
## The following code demonstrates the plotting of confidence intervals
## using predictions from ASReml, back-transformation to measurement scale,
## and how loops can be used to process subsets.

\dontrun{
library(asreml)
library(pascal)
ex <- data.frame(x=rep(0:10,each=2), grp=LETTERS[1:2])
ex$y <- exp(c(1,1.01)[ex$grp]+ex$x*c(.02,.12)[ex$grp]+rnorm(nrow(ex),sd=.2))

ex.asr <- asreml(log(y)~grp*x,data=ex)

plot(ex$y~ex$x,col=c("red","blue")[ex$grp],data=ex)

xvec <- seq(0,10,length=20)
ex.pred <- predict(
    ex.asr,
    classify = "grp:x",
    levels = list(x = xvec),
    newdata = data.frame(
        grp = LETTERS[1:2],
        x = rep(xvec, each = 2)))

## note that in the code below 'g' is in the caller's frame
## whereas 'grp' and 'x' are in 'ex.pred$predictions$pvals'.
for(g in levels(ex$grp))
    plotci.asreml(ex.pred,
                  subset = grp==g,
                  formula = ~x,
                  backfun = exp,
                  ci.f = 2,
                  fill.col = "#00000018",
                  line.col = "black",
                  line.lwd = 2)
}
}
\author{
Pascal Niklaus \email{pascal.niklaus@ieu.uzh.ch}
}
