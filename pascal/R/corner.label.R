#' Plot label in corner of plot
#' 
#' \code{corner.label} plots a label in the corner of a plot, at positions
#' that can be specified in different units.
#' 
#' This function facilitates the labelling of plots. Using \code{grid}-graphics,
#' text is plotted at a distance from one of the plot's corners.
#' These distances can be specified in different units, including relative to
#' the size of characters. In addition, a user-provided function can be called
#' before printing the label.
#' 
#' @param label Text to be plotted, if different from \code{NULL}
#' @param fun If not \code{NULL}, this arbitrary \code{function(x,y,...)} is called
#' to performs some plotting before the text \code{txt} is typeset. See examples.
#' @param pos Corner of plot; can be any combination of "top","bottom","left","right"
#' @param units Units in which distance from plot corner is given. Default is \code{"char"}.
#' Check \code{?unit} for more options.
#' @param dist Distance from corner at which label is plotted. Can be single value or vector \code{(x,y)}.
#' @param ... Extra parameters passed when calling \code{grid.text}. Check \code{?gpar} for options.
#' @examples
#' \dontrun{
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
corner.label <- function(label=NULL,pos="topleft",units="char",dist=1,fun=NULL,...)
{
    requireNamespace("gridBase")
    requireNamespace("grid")

    vps <- gridBase::baseViewports()
    grid::pushViewport(vps$plot)

    dist <- rep(dist,2)[1:2]
    x <- grepl("right",pos)
    y <- grepl("top",pos)    
    xx <- grid::unit(x,"npc") + (.5-x)*grid::unit(dist[1],units)
    yy <- grid::unit(y,"npc") + (.5-y)*grid::unit(dist[2],units)
    
    if(!is.null(fun)) {
        fun(xx,yy,...)
    }
    if(!is.null(label)) {
        grid::grid.text(label,x=xx,y=yy,
                  gp=grid::gpar(...))
    }    
    grid::popViewport()    
}


