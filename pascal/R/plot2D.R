#####################################################################
###
### plotting methods of gfx2D class
###
###

#' Plotting methods for gfx2D objects
#'
#' these mostly are wrappers around corresponding base graphics
#' calls, with coordinate transformations and parameter adjustments
#' applied
#'
#' @name plot2D
#' @rdname plot2D
NULL


######################################################################
###
### lines2D

safeSetGeneric(name = "lines2D",
               def = function(object, ...) standardGeneric("lines2D") )

#' \code{lines2D} draws a set of lines, similar to the regular
#' \code{\link[graphics]{lines}} function.
#'
#' @rdname plot2D
#' @param object object of class gfx2D
#' @param x,y coordinates
#' @param type lines
#' @aliases lines2D lines2D-methods lines2D,gfx2D-method lines2D,gfx2D,ANY-method
#' @export
setMethod(f = "lines2D", signature = "gfx2D",
          definition=function(object, x, y = NULL, type = "l", ...)
          {
              xy <- apply2D(object@m, xy.coords(x,y))
              lines(xy,...)
              invisible(object)
          }
          )


safeSetGeneric(name = "arrows2D",
               def = function(object, ...) standardGeneric("arrows2D") )

#' \code{arrows2D} draws a set of arrows, similar to the regular
#' \code{\link[graphics]{arrows}} function.
#'
#' @rdname plot2D
#' @param x0,y0,x1,y1 coordinates
#' @param length length of arrow head
#' @param ... extra arguments
#' @aliases arrows2D arrows2D-methods arrows2D,gfx2D-method arrows2D,gfx2D,ANY-method
#' @export
setMethod(f = "arrows2D", signature = "gfx2D",
          definition=function(object, x0, y0, x1=x0, y1=y0, length=0.25, ...)
          {
              xy0 <- apply2D(object@m, x0, y0)
              xy1 <- apply2D(object@m, x1, y1)
              cex.scale <- mean( abs( c(object@sx, object@sy) ) )
              arrows(xy0$x, xy0$y, xy1$x, xy1$y, length=length*cex.scale, ...)
              invisible(object)
          }
          )

safeSetGeneric(name = "segments2D",
               def = function(object, ...) standardGeneric("segments2D") )

#' \code{segments2D} draws a set of segments, similar to the regular
#' \code{\link[graphics]{segments}} function.
#'
#' @rdname plot2D
#' @param x0,y0,x1,y1 coordinates
#' @param ... extra arguments
#' @aliases segments2D segments2D-methods segments2D,gfx2D-method segments2D,gfx2D,ANY-method
#' @export
setMethod(f = "segments2D", signature = "gfx2D",
          definition=function(object, x0, y0, x1=x0, y1=y0, ...)
          {
              xy0 <- apply2D(object@m, x0, y0)
              xy1 <- apply2D(object@m, x1, y1)
              cex.scale <- mean( abs( c(object@sx, object@sy) ) )
              segments(xy0$x, xy0$y, xy1$x, xy1$y, ...)
              invisible(object)
          }
          )

######################################################################
###
### rect2D

safeSetGeneric(name = "rect2D",
               def = function(object, ...) standardGeneric("rect2D") )

#' @rdname plot2D
#' @aliases rect2D rect2D,gfx2D-method rect2D,gfx2D,ANY-method
#' @param xleft,ybottom,xright,ytop corners of rectangle
#' @param density,angle density and angle of shading lines (see \code{rect} for details)
#' @param border,col border and fill color of object
#' @param lty,lwd line type and width of object outline
#'
#' code{rect2D} draws one or more rectangles, similar to the regular
#' \code{\link[graphics]{rect}} function.
#'
#' @export
setMethod(
    f = "rect2D", signature = "gfx2D",
    definition = function(object, xleft, ybottom, xright = NULL, ytop = NULL, density = NULL, angle = 45,
                          col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), ...) {
        if (is.null(xright)) xright <- xleft
        if (is.null(ytop)) ytop <- ybottom
        for (i in 1:max(c(length(xleft), length(xright), length(ytop), length(ybottom)))) {
            xl <- recycle(xleft, i)
            xr <- recycle(xright, i)
            yt <- recycle(ytop, i)
            yb <- recycle(ybottom, i)
            xy <- apply2D(
                object@m,
                list(
                    x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb)
                )
            )
            polygon(xy,
                density = recycle(density, i),
                angle = recycle(angle, i),
                col = recycle(col, i),
                border = recycle(border, i),
                lty = recycle(lty, i),
                lwd = recycle(lwd, i),
                ...
            )
        }
        invisible(object)
    }
)

######################################################################
###
### polygon2D

safeSetGeneric(name = "polygon2D",
               def = function(object, ...) standardGeneric("polygon2D") )

#' @rdname plot2D
#' @aliases polygon2D
#' @aliases polygon2D,gfx2D-method
#' @aliases polygon2D,gfx2D,ANY-method
#'
#' \code{polygon2D} draws a polygon, similar to the
#' \code{polygon} function.
#'
#'
#' @export
setMethod(f = "polygon2D", signature = "gfx2D",
          definition=function(object, x, y = NULL, ...)
          {
              xy <- apply2D(object@m, xy.coords(x,y))
              polygon(xy,...)
          })

######################################################################
###
### points2D

safeSetGeneric(name = "points2D",
               def = function(object, ...) standardGeneric("points2D") )

#' @rdname plot2D
#' @aliases points2D
#' @aliases points2D,gfx2D-method
#' @aliases points2D,gfx2D,ANY-method
#' @export
setMethod(f = "points2D", signature = "gfx2D",
          definition=function(object, x, y = NULL, ...)
          {
              xy <- apply2D(object@m, xy.coords(x,y))
              points(xy,...)
          })


######################################################################
###
### text2D

safeSetGeneric(name = "text2D",
               def = function(object, ...) standardGeneric("text2D") )

#' @rdname plot2D
#' @aliases text2D text2D,gfx2D-method text2D,gfx2D,ANY-method
#' @param labels text to write
#' @param adj,pos,offset text positioning options, as explained in
#'     detail \code{text}
#' @param vfont,font font write text
#' @param srt text rotation in degrees; note that this is the rotation
#'     that is in addition to any rotation specified in the
#'     transformation matrix
#' @param cex character enlargement; note tht this is the enlargement
#'     that is applied in addition to any scaling specified in the
#'     transformation matrix
#' @export
setMethod(f = "text2D", signature = "gfx2D",
          definition = function(object, x, y = NULL, labels = seq_along(x$x),
                                adj = NULL, pos = NULL, offset = 0.5, vfont = NULL,
                                cex = 1, col = NULL, font = NULL, srt=0, ...)
          {
              xy <- apply2D( object@m, xy.coords(x, y) )
              rot <- object@rot * 180 / pi
              cex.scale <- mean( abs( c(object@sx, object@sy) ) )
              for(i in seq_along(xy$x) )
                  text(x = recycle(xy$x, i), y = recycle(xy$y, i),
                       labels = recycle(labels, i),
                       adj = recycle(adj, i), pos = recycle(pos, i), offset = recycle(offset, i),
                       vfont = vfont, font = recycle(font, i),
                       col = recycle(col, i), cex = recycle(cex, i) * cex.scale, srt = recycle(srt, i) - rot, ...)
          })

######################################################################
###
### circle2D

safeSetGeneric(name = "circle2D",
               def = function(object, ...) standardGeneric("circle2D") )

#' Circle2D
#'
#' shrt of circle2d
#'
#' long of circle2d
#'
#' @rdname plot2D
#' @aliases circle2D
#' @aliases circle2D,gfx2D-method
#' @aliases circle2D,gfx2D,ANY-method
#' @param r,rx,ry radii of ellipse. If only \code{r} or \code{rx} are
#'     specified, a circle is drawn. If \code{rx} and \code{ry} are
#'     specified, an ellipse with these major radii is drawn.
#' @param phi angle by which the ellipse is rotated, in radians
#' @param n number of corners of polygon (chose a large number to approximate a circle)
#' @export
setMethod(
    f = "circle2D", signature = "gfx2D",
    definition = function(object, x, y = NULL, r = NULL, rx = r, ry = rx, pos = "center", phi = 0, n = 16,
                          density = NULL, angle = 45, col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), ...) {
        ## xy <- apply2D(object@m, xy.coords(x, y))
        xy <- xy.coords(x, y)
        for (i in seq_along(xy$x)) {
            polygon(
                apply2D(object@m,polycircle(xy$x[i], xy$y[i],
                    r = recycle(r, i), rx = recycle(rx, i), ry = recycle(ry, i),
                    pos = recycle(pos, i), phi = recycle(phi, i), n = recycle(n, i)
                )),
                density = recycle(density, i),
                angle = recycle(angle, i),
                col = recycle(col, i),
                border = recycle(border, i),
                lty = recycle(lty, i),
                lwd = recycle(lwd, i),
                ...
            )
        }
    }
)
## xspline2D



## function (x, y = NULL, shape = 0, open = TRUE, repEnds = TRUE,
##     draw = TRUE, border = par("fg"), col = NA, ...)

## arc2D

## ellipse2D


## quick and dirty hack to store NULL in data frames
##.fixNULL <- function(x) if(is.null(x)) "!null!" else if(is.na(x)) NA else if(x=="!null!") NULL else x

#' @rdname plot2D
#' @export
demo2D <- function()
{
    plot(NULL,xlim=c(-20,20),ylim=c(-20,20),xlab="x",ylab="y",asp=1)
    star.deg <- 0:9*pi/5
    star <- list(x=cos(star.deg)*c(.8,2), y=sin(star.deg)*c(.8,2))
    g <- gfx2D()
    h <- gfx2D()
    for(i in 1:10) {
        transformation(g) <- rotate2D(pi*i/10) %*% scale2D(sqrt(i)) %*% translate2D(sqrt(i),sqrt(i))
        transformation(h) <- rotate2D(0.5*pi/(5+i)) %*% scale2D(.5*sqrt(i)) %*% translate2D(i,i)
        polygon2D(g,star,col="#ff000030")
        text2D(h,0,5,sprintf("Text %d",i),col="#0000ff80")
        text2D(h,-5,5,sprintf("Text %d",i),cex=.5,srt=45,col="#0000ff80")
    }
}
