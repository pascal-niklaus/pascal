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
#'
#' @param fun If not \code{NULL}, this arbitrary
#'     \code{function(x,y,...)} is called to performs some plotting
#'     before the text \code{txt} is typeset. See examples.
#'
#' @param pos Corner of plot; can be any combination of
#'     "top","bottom","left","right"
#'
#' @param units Units in which distance from plot corner is
#'     given. Default is \code{"char"}.  Check \code{?unit} for more
#'     options.
#'
#' @param dist Distance from corner at which label is plotted. Can be
#'     single value or vector \code{(x,y)}.
#'
#' @param frame Either of 'inner', 'figure', or 'plot'. Indicates the
#'     area that is to be labelled. 'inner' is the entire page area
#'     except the margins. 'figure' is the figure area. 'plot' is the
#'     area used for plotting (inside the axis). Defaults to 'plot'.
#'
#' @param ... Extra parameters passed when calling
#'     \code{grid.text}. Check \code{?gpar} for options.
#'
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
corner.label <- function(label=NULL,pos="topleft",units="char",dist=1,fun=NULL,frame="plot",...)
{
    requireNamespace("gridBase")
    requireNamespace("grid")

    frame.opts <- c("inner","figure","plot")
    frame <- which(frame==frame.opts)
    if(length(frame)==0)
        stop("frame must be one of ",paste(paste("'",frame.opts,"'",sep=""),collapse=", "))

    vps <- gridBase::baseViewports()
    for(i in 1:frame)
        grid::pushViewport(vps[[i]]
                       )
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
    grid::popViewport(n=i)
}

#' Add label to plot
#'
#' \code{label} plots a label at positions that can be specified in
#' different units.
#'
#' This function facilitates the labelling of plots. Using
#' \code{grid}-graphics, text is plotted somewhere in or outside the
#' plot.  These position can be specified in different units,
#' including relative to the size of characters. In addition, a
#' user-provided function can be called before printing the
#' label. This could, for example, be used to first draw a background.
#'
#' @param x,y Coordinates of the labels, or a string containing a
#'     combination of dimensions and units (e.g. "2native+2.5lines")
#'
#' @param label Text to be plotted
#'
#' @param fun If not \code{NULL}, this arbitrary
#'     \code{function(x,y,...)} is called to performs some plotting
#'     before the text \code{txt} is typeset. See examples.
#'
#' @param just Justification of label. Defaults to "center"
#'
#' @param rot Angle at which label is printed. Defaults to zero.
#'
#' @param units Units of the coordinates of the labels. Default is
#'     \code{"native"} for X and \code{"lines"} for Y. Check
#'     \code{?grid::unit} for the many options available.
#'
#' @param frame Either of 'inner', 'figure', or 'plot'. Indicates the
#'     area that is to be labelled. 'inner' is the entire page area
#'     except the margins. 'figure' is the figure area. 'plot' is the
#'     area used for plotting (inside the axis). Defaults to 'plot'.
#'
#' @param ... Extra parameters passed when calling
#'     \code{grid.text}. Check \code{?gpar} for options.
#'
#' @examples
#' \dontrun{
#'
#' #### Simple bar plot example
#'
#' ## create bar plot with error bars; the location of the bar is returned by 'barplot'
#' d <- aggr(CO2, c("Type","Treatment"), c("uptake=mean(uptake)","se=se(uptake)"))
#' d$x <- barplot(d$uptake, space=groupSpace(d, "Type")*.5, ylim=c(0,40), ylab="")
#' xy.errbar(d$x, d$uptake, yerr =d$se, cap=0.08, add=TRUE)
#'
#' ## calculate label positions
#' d2 <- aggr(d, "Type", "x=mean(x)")
#' label(d2$x, -1.5, d2$Type)
#'
#' ## add axes labels
#' label(mean(d2$x),-2.5, "Type", fontsize=15, fontface="bold")
#' label(-2.5,20, units=c("lines","native"), "Uptake", fontsize=15, fontface="bold", rot=90)
#'
#' #### Example of how to use th 'fun' argument
#'
#' plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab="", ylab="")
#' x <- runif(12)
#' y <- runif(12)
#' for(i in 1:12) {
#'     label(x[i], y[i], month.name[i],
#'           units=c("native", "native"),
#'           fun=function(x, y, txt,...)
#'               grid::grid.rect(
#'                         x, y,
#'                         width=grid::unit(1,"strwidth",txt)+grid::unit(1,"char"),
#'                         height=grid::unit(1,"strheight", txt)+grid::unit(1,"char"),
#'                         gp=grid::gpar(col="red",fill="yellow")),
#'           col="blue", fontface="bold")
#' }
#' }
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
label <- function(x, y, label=NULL, units=c("native", "lines"),
                         just = "center", rot=0,
                         fun=NULL, frame="plot", ...) {
    requireNamespace("gridBase")
    requireNamespace("grid")

    frame.opts <- c("inner", "figure", "plot")
    frame <- which(frame == frame.opts)
    if (length(frame) == 0)
        stop("frame must be one of ",
             paste(paste("'", frame.opts, "'", sep = ""),
                   collapse = ", "))

    vps <- gridBase::baseViewports()

    for (i in 1:frame)
        grid::pushViewport(vps[[i]])

    if (is.numeric(x))
        xx <- grid::unit(x, units[1])
    else
        xx <- .processunits(x)

    if (is.numeric(y))
        yy <- grid::unit(y, units[2])
    else
        yy <- .processunits(y)

    if (!is.null(fun))
        fun(xx, yy, label, ...)
    if (!is.null(label))
        grid::grid.text(label, x = xx, y = yy, rot = rot, just = just,
                        gp = grid::gpar(...))
    grid::popViewport(n = i)
}

#' Add a line to a plot using grid coordinates
#'
#' \code{gline} draws a line with coordinates that can be specified in
#' different units.
#'
#' This function facilitates the decoration of plots. Using
#' \code{grid}-graphics, lines are drawn using coordinates that can be
#' specified in different units.
#'
#' @param x,y Coordinates of the labels, or a string containing a
#'     combination of dimensions and units
#'     (e.g. "2native+2.5lines"). Coordinate vectors are recycled to
#'     equal length if necessary, allowing to pass just a single x or y value
#'     for vertical and horizontal lines, respectively.
#'
#' @param frame Either of 'inner', 'figure', or 'plot'. Indicates the
#'     area that is to be labelled. 'inner' is the entire page area
#'     except the margins. 'figure' is the figure area. 'plot' is the
#'     area used for plotting (inside the axis). Defaults to 'plot'.
#'
#' @param ... Extra parameters passed when calling
#'     \code{grid.line}. Check \code{?gpar} for options.
#'
#' @examples
#' \dontrun{
#' plot(NULL, xlim=c(0,2), ylim=c(1,3))
#' gline(c("0.5native","1.5native"), "-1lines", col="blue",lwd=2)
#' }
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
gline <- function(x, y, frame="plot", ...) {
    requireNamespace("gridBase")
    requireNamespace("grid")

    n <- max(lengths(list(x, y)))
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)

    frame.opts <- c("inner", "figure", "plot")
    frame <- which(frame == frame.opts)
    if (length(frame) == 0)
        stop("frame must be one of ",
             paste(paste("'", frame.opts, "'", sep = ""),
                   collapse = ", "))

    vps <- gridBase::baseViewports()

    for (i in 1:frame)
        grid::pushViewport(vps[[i]])

    grid::grid.move.to(.processunits(x[1]),
                        .processunits(y[1]))
    for (i in 2:length(x)) {
        grid::grid.line.to(.processunits(x[i]),
                            .processunits(y[i]),
                            gp = grid::gpar(...))
    }
    grid::popViewport(n = i)
}


.processunits <- function(x) {
    if (is.character(x)) {
        x <- gsub("([0-9.]+)([a-z]+)",
                  "grid::unit(\\1,\"\\2\")",
                  x,
                  perl = TRUE)
        v <- eval(parse(text = x))
    } else if (is.list(x)) {
        for (i in seq_along(x)) {
            vv <- grid::unit(x[[1]], units = names(x)[1])
            v <- if (i == 1) v else v + vv
        }
    } else {
        stop("illegal format of dimension")
    }
    v
}
