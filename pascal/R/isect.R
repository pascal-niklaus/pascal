#' Intersect two lines
#'
#' Calculate the point (x,y) at which two lines intersect. Note that the specification
#' or the line is organised differently than in e.g. \code{arrows}.
#'
#' @param x1 Vector of two specifying the end point x coordinates of
#'     first line
#' @param y1 Vector of two specifying the end point y coordinates of
#'     first line
#' @param x2 Vector of two specifying the end point x coordinates of
#'     second line
#' @param y2 Vector of two specifying the end point y coordinates of
#'     second line
#' @param segment Logical determining whether the the lines are
#'     segments ending at the end points specified, or whether the
#'     lines have infinite length.
#' @param eps tolerance for numeric comparisions of whether point lies
#'     within segment. This is important for horizontal or vertical
#'     lines where the x or y coordinates span no range.
#'
#' @return Vector with coordinates of intersection \code{x,y}. These coordinates equal
#'         NA if the segment option is specified and the segments don't intersect
#' @examples
#' isect(c(0,1),c(0,1),c(0,1),c(1,0))
#' ## [1] 0.5 0.5
#' isect(c(0,1),c(0,1),c(0,3),c(1,2))
#' ## [1] NA NA
#' isect(c(0,1),c(0,1),c(0,3),c(1,2),segment=FALSE)
#' ## [1] 1.5 1.5
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords misc, utilities
#' @export
isect <- function(x1,y1,x2,y2,segment=T, eps=1e-6) {
  xi <- ((x1[1]*y1[2]-y1[1]*x1[2])*(x2[1]-x2[2])-(x1[1]-x1[2])*(x2[1]*y2[2]-y2[1]*x2[2])) /
         ((x1[1]-x1[2])*(y2[1]-y2[2])-(y1[1]-y1[2])*(x2[1]-x2[2]))
  yi <- ((x1[1]*y1[2]-y1[1]*x1[2])*(y2[1]-y2[2])-(y1[1]-y1[2])*(x2[1]*y2[2]-y2[1]*x2[2])) /
         ((x1[1]-x1[2])*(y2[1]-y2[2])-(y1[1]-y1[2])*(x2[1]-x2[2]))
  if(segment & (
     xi+eps < min(x1) || xi+eps < min(x2) ||
     xi-eps > max(x1) || xi-eps > max(x2) ||
     yi+eps < min(y1) || yi+eps < min(y2) ||
     yi-eps > max(y1) || yi-eps > max(y2))) {
    xi <- NA
    yi <- NA
  }
  return (c(xi,yi))
}
