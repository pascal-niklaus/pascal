#' Area of polygon
#' 
#' Calculate area of polygon.
#' 
#' The area of the polygon is calculated using the shoelace method,
#' after translation towards the origin to improve numeric accurary.
#' The polygon passed can be a list of subpolygons. 
#'
#' Simple polygons are lists and must have elements \code{x} and
#' \code{y}. More complex shapes are lists of polygons. Polygons with
#' clock-wise orientation have positive area, whereas polygons with
#' counter-clockwise orientation are holes (negative area). Polygons
#' cannot be self-intersecting.
#'
#' These data structures are compatible with the
#' \code{polyclip} library.
#'
#' @param x polygon
#' 
#' @return The area of the polygon
#' 
#' @examples
#' a <- polyrect(2,3,4,5)
#' b <- polycircle(2,3,r=1,n=64)
#' polyarea(a)
#' polyarea(b)
#' \dontrun{
#' library(polyclip)
#' c <- polyclip(a,b,op="minus")
#' polyarea(c)
#' }
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
polyarea <- function(x)
{
    if(is.list(x[[1]]))  {
        sum(sapply(x, function(p) polyarea2(p)))
    } else
        polyarea2(x)
}

polyarea2<- function(a)
{
    x <- a$x - mean(a$x)
    y <- a$y - mean(a$y)    
    idx2 <- c(2:length(x),1)
    sum(x*y[idx2]-x[idx2]*y)/2
}

## #' Generate polygon
## #' 
## #' Create a polygon with clock-wise orientation
## #' 
## #' The area of the polygon is calculated using the shoelace method,
## #' after translation towards the origin to improve numeric accurary.
## #' The polygon passed can be a list of subpolygons. 
## #'
## #' Polygons are lists with elements \code{x} and \code{y}, and
## #' clock-wise orientation. These data structures are compatible with
## #' the \code{polyclip} library.
## #'
## #' @param x center of polygon, either as vector \code{c(x,y)} or
## #'     \code{x} alone if \code{y} is given.
## #'
## #' @param y coordinate
## #'     \code{y} of polygon, unless \code{x} is a vector
## #'
## #' @param r radius of circle
## #'
## #' @param rx ellipse radius in x direction
## #'     (before rotation)
## #'
## #' @param ry ellipse radius in y direction
## #'     (before rotation)
## #'
## #' @param w width
## #'
## #' @param h height
## #'
## #' @param n number of points that are used to construct circle or ellipse
## #'
## #' @param phi angle to rotate in clockwise
## #'     direction
## #'
## #' @param pos anchor point of polygon for which the
## #'     coordinate (x,y) is given and around which the object is
## #'     rotated. Can be any meaningful combination of
## #'     "top","bottom","left", and "right". These can also be
## #'     abbreviated as "T","B","L","R". Default is "center".
## #' 
## #' @return polygon object
## #' 
## #' @examples
## #' plot(c(),c(),xlim=c(-6,6),ylim=c(-6,6),asp=1,xlab="x",ylab="y")
## #' for(phi in seq(0,2*pi,length=16))
## #'    polygon(polycircle(0,0,rx=3,ry=1,pos="left",phi=phi,n=60),col="#ff000040")
## #' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
## #' @aliases poly2D
## #' @seealso affine2D plot2D
## #' @rdname polyrect
## #' @export
## polycircle <- function(x,y=NULL,r=NULL,rx=r,ry=rx,pos="center",phi=0,n=16)
## {
##     yadj <- - grepl("(T|top)",pos,perl=TRUE) + grepl("(T|bottom)",pos,perl=TRUE)
##     xadj <- grepl("(L|left)",pos,perl=TRUE) - grepl("(R|right)",pos,perl=TRUE)
##     if(is.null(y)) {
##         y <- x[2]
##         x <- x[1]
##     }
##     ang <- seq(0, by = 2*pi/n, length.out = n)
##     p <- matrix(c(cos(ang),sin(ang),rep(1,n)), nrow=3, byrow = TRUE)
##     m <- translate2D(x,y) %*%
##         rotate2D(phi) %*%
##         scale2D(rx,ry) %*%
##         translate2D(xadj,yadj)
##     p <- m %*% p
##     list(x = p[1,],
##          y = p[2,])       
## }

## #' @rdname polyrect
## #' @export
## polystar <- function(x,y=NULL,r=NULL,r1=r,r2=r/2,phi=0,n=5)
## {
##     if(is.null(y)) {
##         y <- x[2]
##         x <- x[1]
##     }
##     ang <- seq(0, by = pi/n, length.out = n*2)
##     list(x = x + cos(ang+phi)*c(r1,r2),
##          y = y + sin(ang+phi)*c(r1,r2))
## }

## .square <- matrix(c(.5,-.5,1,.5,.5,1,-.5,.5,1,-.5,-.5,1),nrow=3)

## #' @rdname polyrect
## #' @export
## polyrect <- function(x,y,w,h,pos="center",phi=0)
## {
##     yadj <- - grepl("(T|top)",pos,perl=TRUE)*.5 + grepl("(T|bottom)",pos,perl=TRUE)*.5
##     xadj <- grepl("(L|left)",pos,perl=TRUE)*.5 - grepl("(R|right)",pos,perl=TRUE)*.5
##     m <- translate2D(x,y) %*% rotate2D(phi) %*% scale2D(w,h) 
##     p <- m %*% .square
##     list(x = p[1,],
##          y = p[2,])
## }

