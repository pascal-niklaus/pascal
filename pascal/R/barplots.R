#' Add spacing between bars in bar plot
#'
#' \code{groupSpace} is a helper function to add space between the
#' bars of a bar plot.
#'
#' Given a \code{data.frame} and the names of columns in it, it
#' determines transitions between the groups these columns define and
#' returns 1 for a transition, or 0 otherwise. These values can be
#' passed to the \code{space} argument of \code{barplot}.
#'
#' @param d data frame with data
#'
#' @param cols the names of the columns that define the groups.
#'
#' @return A vector with values of 1 when a transition occurs.
#'
#' @examples
#' \dontrun{
#'
#'
#'
#' library(grid) # required only because we use grid.cirle in fun
#' plot(rnorm(10),rnorm(10))
#' corner.label("a",pos="topleft",dist=2,cex=2)
#' corner.label(label="b",pos="topright",
#'              fun=function(x,y,...)
#'                  grid.circle(x,y,unit(.7,"char"),
#'                              gp=gpar(fill="black",...)),
#'              dist=2,units="cm",
#'              col="white",
#'              fontface="bold",
#'              fontsize=30)
#' corner.label("c",pos="bottomright",cex=2)
#' corner.label("ddd",pos="bottomleft",dist=c(1.7,1),cex=2)
#' }
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
groupSpace <- function(d, cols) {
    tmp <- apply(d[,which(names(d) %in% cols), drop=FALSE], 1,
                 function(x) paste(x,collapse="|"))
    n <- length(tmp)
    c(0, ifelse(tmp[-n] == tmp[-1], 0, 1))
}
