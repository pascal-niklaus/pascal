% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corner.label.R
\name{corner.label}
\alias{corner.label}
\title{Plot label in corner of plot}
\usage{
corner.label(
  label = NULL,
  pos = "topleft",
  units = "char",
  dist = 1,
  fun = NULL,
  frame = "plot",
  ...
)
}
\arguments{
\item{label}{Text to be plotted, if different from \code{NULL}}

\item{pos}{Corner of plot; can be any combination of
"top","bottom","left","right"}

\item{units}{Units in which distance from plot corner is
given. Default is \code{"char"}.  Check \code{?unit} for more
options.}

\item{dist}{Distance from corner at which label is plotted. Can be
single value or vector \code{(x,y)}.}

\item{fun}{If not \code{NULL}, this arbitrary
\code{function(x,y,...)} is called to performs some plotting
before the text \code{txt} is typeset. See examples.}

\item{frame}{Either of 'inner', 'figure', or 'plot'. Indicates the
area that is to be labelled. 'inner' is the entire page area
except the margins. 'figure' is the figure area. 'plot' is the
area used for plotting (inside the axis). Defaults to 'plot'.}

\item{...}{Extra parameters passed when calling
\code{grid.text}. Check \code{?gpar} for options.}
}
\description{
\code{corner.label} plots a label in the corner of a plot, at positions
that can be specified in different units.
}
\details{
This function facilitates the labelling of plots. Using \code{grid}-graphics,
text is plotted at a distance from one of the plot's corners.
These distances can be specified in different units, including relative to
the size of characters. In addition, a user-provided function can be called
before printing the label.
}
\examples{
\dontrun{
library(grid) # required only because we use grid.cirle in fun
plot(rnorm(10),rnorm(10))
corner.label("a",pos="topleft",dist=2,cex=2)
corner.label(label="b",pos="topright",
             fun=function(x,y,...)
                 grid.circle(x,y,unit(.7,"char"),
                             gp=gpar(fill="black",...)),
             dist=2,units="cm",
             col="white",
             fontface="bold",
             fontsize=30)
corner.label("c",pos="bottomright",cex=2)
corner.label("ddd",pos="bottomleft",dist=c(1.7,1),cex=2)
}
}
\author{
Pascal Niklaus \email{pascal.niklaus@ieu.uzh.ch}
}
