#' Generate polygons
#' 
#' Create a polygon with clock-wise orientation
#' 
#' \code{polyrect} creates a rectangle with width \code{w}, height
#' \code{h}, and origin (\code{x}, \code{y}). The origin can be
#' anchored to the center of the rectangle (default), or and corner
#' and side (see argument \code{pos} for details). The rectangle can
#' be rotated around the origin by angle \code{phi}.
#' 
#' \code{polycircle} creates a regular polygon with \code{n} (default
#' 16) corners. This approximates a circle, but can also be used to
#' draw regular polygons when a small n is chosen. Chosing radii that
#' are different in x and y direction results in an ellipse. The
#' ellipse can be rotated around an anchor. Anchor and origin are
#' specified as for the \code{polyrect}.  Note that placing an anchor
#' to a corner refers to the enclosing rectangle, with the anchor
#' being away from the actual ellipse.
#' 
#' \code{polystar} creates a star with \code{n} tips. Tips and
#' inter-tip nodes are placed at distance \code{r1} and \code{r2} from
#' the origin. A rotation by angle \code{phi} can be applied for
#' proper tip orientation.
#'  
#' Polygons are lists with elements \code{x} and \code{y}, and
#' clock-wise orientation. These data structures are compatible with
#' the \code{polyclip} library.
#'
#' @param x center of polygon, either as vector \code{c(x,y)} or
#'     \code{x} alone if \code{y} is given.
#'
#' @param y coordinate
#'     \code{y} of polygon, unless \code{x} is a vector
#'
#' @param r,r1,r2 radius of circle. For stars, \code{r1} and \code{r2}
#'     indicate the location of the tips and inner nodes of the star.
#'
#' @param rx ellipse radius in x direction
#'     (before rotation)
#'
#' @param ry ellipse radius in y direction
#'     (before rotation)
#'
#' @param w width
#'
#' @param h height
#'
#' @param n number of points that are used to construct circle or ellipse
#'
#' @param phi angle to rotate in clockwise
#'     direction
#'
#' @param pos anchor point of polygon for which the
#'     coordinate (x,y) is given and around which the object is
#'     rotated. Can be any meaningful combination of
#'     "top","bottom","left", and "right". These can also be
#'     abbreviated as "T","B","L","R". Default is "center".
#' 
#' @return polygon object
#' 
#' @examples
#' plot(c(),c(),xlim=c(-6,6),ylim=c(-6,6),asp=1,xlab="x",ylab="y")
#' for(phi in seq(0,2*pi,length=16))
#'    polygon(polycircle(0,0,rx=3,ry=1,pos="left",phi=phi,n=60),col="#ff000040")
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @aliases poly2D
#' @seealso affine2D plot2D
#' @rdname polycircle
#' @export
polycircle <- function(x,y=NULL,r=NULL,rx=r,ry=rx,pos="center",phi=0,n=16)
{
    yadj <- - grepl("(T|top)",pos,perl=TRUE) + grepl("(T|bottom)",pos,perl=TRUE)
    xadj <- grepl("(L|left)",pos,perl=TRUE) - grepl("(R|right)",pos,perl=TRUE)
    if(is.null(y)) {
        y <- x[2]
        x <- x[1]
    }
    ang <- seq(0, by = 2*pi/n, length.out = n)
    p <- matrix(c(cos(ang),sin(ang),rep(1,n)), nrow=3, byrow = TRUE)
    m <- translate2D(x,y) %*%
        rotate2D(phi) %*%
        scale2D(rx,ry) %*%
        translate2D(xadj,yadj)
    p <- m %*% p
    list(x = p[1,],
         y = p[2,])       
}

#' @rdname polycircle
#' @export
polystar <- function(x,y=NULL,r=NULL,r1=r,r2=r/2,phi=0,n=5)
{
    if(is.null(y)) {
        y <- x[2]
        x <- x[1]
    }
    ang <- seq(0, by = pi/n, length.out = n*2)
    list(x = x + cos(ang+phi)*c(r1,r2),
         y = y + sin(ang+phi)*c(r1,r2))
}

.square <- matrix(c(.5,-.5,1,.5,.5,1,-.5,.5,1,-.5,-.5,1),nrow=3)

#' @rdname polycircle
#' @export
polyrect <- function(x,y,w,h,pos="center",phi=0)
{
    yadj <- - grepl("(T|top)",pos,perl=TRUE)*.5 + grepl("(T|bottom)",pos,perl=TRUE)*.5
    xadj <- grepl("(L|left)",pos,perl=TRUE)*.5 - grepl("(R|right)",pos,perl=TRUE)*.5
    m <- translate2D(x,y) %*% rotate2D(phi) %*% scale2D(w,h) 
    p <- m %*% .square
    list(x = p[1,],
         y = p[2,])
}

