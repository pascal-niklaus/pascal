#' @rdname triangle-class
#' @title Triangle Class.
#' @name triangle-class
#' @exportClass triangle
setClass("triangle",
         slots = c(sides = "numeric", angles = "numeric",
                   x = "numeric", y = "numeric",
                   degrees = "logical", k = "numeric"),
         prototype = prototype(degrees=TRUE),
         validity = function(object) return(TRUE))

#' Triangle geometry
#'
#' The class \code{triangle} allows to determine and manipulate
#' triangles. \code{triangle} is a generator for an object of class
#' \code{triangle}.  The triangle can be defined by a combination of
#' side lengths and corner angles, or by the corner coordinates. A
#' range of class methods then allow to manipulate the resulting
#' triangle object.
#'
#' Sides, angles, and coordinates must have matching orders. Note
#' that angles are opposite to the corresponding sides.
#'
#' If corner coordinates \code{x} and \code{y} are passed, and
#' \code{side} and \code{angle} argument is ignored and these are
#' re-calculated from the coordinates.
#'
#' Alternatively, \code{sides} and \code{angles} can be provided, with
#' unknown elements passed as \code{NA}. Any combination that defines
#' a triangle is valid. Specifically, these can be all three sides, a
#' side and two angles, two angles and a side, and three angles and a
#' side. Note that for a single angle that is not between the two
#' sides, there can also be no or two solutions.
#'
#'
#' Method to extract basic data:
#'
#' \code{sides} and \code{angles} extract the sides and angles. The
#' angles are returned in degrees if the degrees flag was set to TRUE
#' during triangle construction.
#'
#' \code{circumference} and \code{area} yield the corresponding quantity.
#'
#' \code{as.data.frame} returns a data frame with colums with the
#' corner coordinates, angles, and side lengths.
#'
#'
#' Methods to extract geometric data:
#'
#' \code{circumcircle} and \code{incircle} return a list with radius
#' and center of these circles.
#'
#'
#' Methods to manipulate triangle:
#'
#' \code{grow} grows (or shrinks) a triangle by adding a margin to each side.
#'
#' \code{place} rotates the triangle by \code{angle}, then shifts it
#' by \code{origin}.
#'
#' \code{transform2D} applies a 2D affine transformation to the
#' triangle. The geometry (sides, angles) are adjusted accordingly.
#'
#'
#' Methods to plot the triangle:
#'
#' \code{plot} plots the triangle. Circumcircle and incircle can also
#' be shown.
#'
#'
#' @param sides vector with the length of the three sides a, b and c.
#' @param angles vector with the three angles alpha, beta, and
#'     gamma. alpha is opposite to a, etc.
#' @param degrees logical indicating whether the angles are in
#'     degrees. This setting is remembered and any future angles are
#'     output in degrees, if this flag is true.
#' @param x,y coordinates of corners of triangle. If these are
#'     specified, they override any specification of sides and angles.
#'     (exception: \code{x} is a triangle object in case of
#'     \code{as.data.frame} and \code{plot})
#' @param simplify logical indicating whether a list instead of a list
#'     of lists should be returned in case there is only a single
#'     solution.
#'
#' @return A list of triangle objects (none, one or two), or a single
#'     triangle object in case of \code{simplify=TRUE} and a unique
#'     solution.
#'
#' @param object triangle object
#' @param which which parts to plot. 1= triangle, 2=incircle,
#'     3=circumcircle
#' @param add logical indicating whether the elements should be added
#'     to an existing plot
#' @param margins margin to add (positive values) or subtract
#'     (negative values). This can be a scalar or a vector or three
#' @param angle angle by which triangle is rotated (angle=0 indicates
#'     that side AB is along the positive X axis; the angle is
#'     measured counter-clock wise)
#' @param origin coordinate (x,y) by which triangle is shifted. (0,0)
#'     indicates that corner A is at the origin.
#' @param m 2D affine transformation matrix
#' @param row.names row names to add when object is coerced to a data
#'     frame
#' @param optional unused
#' @param  \dots additional parameters
#'
#' @examples
#'
#' ## three sides
#' t1 <- triangle(sides=c(3,4,5))
#' t2 <- grow(t1, margins=c(1,0,0))
#' incircle(t2)
#' t3 <- transform2D(t2, scale2D(2)%*%rotate2D(30*pi/180)%*%translate2D(5,5))
#' plot(t3)
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname triangle-class
#' @export
triangle <- function(sides=rep(NA,3),
                     angles=rep(NA,3),
                     x=NULL,
                     y=NULL,
                     degrees = TRUE,
                     simplify = TRUE)
{
    ## TODO: This function could benefit from some code clean-up. The
    ## multiple exit points are not very nice...

    ## if coordinates are passed, construct triangle from these
    if(!is.null(x)) {
        tri <- new("triangle")
        tri@degrees <- degrees
        return(.geometry_from_xy(tri,x,y))
    }

    ## conversion factor from degrees to radians, if angles in degrees
    if(degrees)
        angles <- angles*pi/180

    ## known and unknown sides
    ik <- which(is.finite(sides))
    iu <- which(!is.finite(sides))

    ## known and unknown angles
    ak <- which(is.finite(angles))
    au <- which(!is.finite(angles))

    if(length(ak)==3 && all.equal(sum(angles),pi)!=TRUE)
        stop("Sum of angles is not 180 degrees!")

    if(length(ak)<3 && length(ak)>1 && sum(angles[ak]) >= pi)
        stop("Sum of angles provided is too large!")

    if(length(ik) == 3) {
        ## three sides known
        angles <- .angles_from_sides(sides)
    } else if(length(ik == 2) && length(ak) == 1) {
        ## two sides and one angle known
        if(! ak %in% ik) {
            ## two sides and angle between -> calculate last side
            sides[iu] <- sqrt(sum(sides[ik]^2)-2*prod(sides[ik])*cos(angles[iu]))
            angles[ik] <- .angles_from_sides(sides)[ik]
        } else {
            ## two sides and angle at one side. This generally has two solutions
            t1 <- setdiff(ik,ak)
            t0 <- setdiff(ik,t1)
            t2 <- setdiff(1:3,ik)

            asinarg <- sin(angles[t0])/sides[t0]*sides[t1]
            if(asinarg > 1) # no solution exists
                return(NULL)
            tmp <- lapply(
                if(1-asinarg < 1e-6) 0 else c(pi,0),
                function(a) {
                    angles[t1] <- abs(a-asin(asinarg))
                    angles[t2] <- pi-sum(angles[ik])
                    sides[t2] <- sides[t0]/sin(angles[t0])*sin(angles[t2])
                    list(sides=sides, angles=angles, degrees=degrees)
                })
            if(length(tmp) == 1) {
                sides <- tmp[[1]]$sides
                angles <- tmp[[1]]$angles
            } else {
                return(invisible(lapply(tmp,
                              function(e) {
                                  tri <- new("triangle")
                                  tri@sides <- e$sides
                                  tri@angles <- e$angles
                                  tri@degrees <- degrees
                                  place(tri)
                              })))
            }
        }
    } else if(length(ik)==1 && length(ak)==2) {
        ## one side and two angles
        ## add missing angle
        angles[au] = pi-sum(angles[ak])
        ## calculate missing sides
        sides[iu] <- sides[ik] * sin(angles[iu])/sin(angles[ik])
    } else if(length(ak)==3 && length(ik)==1) {
        ## three angles and one side that provides scale
        sides[iu] <- sin(angles[iu])*sides[ik]/sin(angles[ik])
    } else
        stop("Don't know how to find solution!")
    tri <- new("triangle")
    tri@degrees <- degrees
    tri@sides <- sides
    tri@angles <- angles
    tri <- place(tri)
    if(simplify)
        invisible(tri)
    else
        invisible(list(tri))
}

######################################################################
###
### grow

safeSetGeneric(name = "grow",
               def = function(object, ...) standardGeneric("grow") )

#' @rdname triangle-class
#' @aliases grow, triangle-method grow
#' @export
setMethod(f = "grow", signature = "triangle",
          definition = function(object, margins)
          {
              margins <- rep(margins,3/length(margins))
              ## original corners
              pts <- lapply(1:3, function(i) c(object@x[i],object@y[i]))
              ## shifted corners
              pts <-
                  lapply(1:3,
                         function(i) {
                             pts[[i]] +
                                 rowSums(
                                     sapply(
                                         setdiff(1:3, i),
                                         function(r)
                                             margins[r] / sin(object@angles[i]) *
                                             .unitvec(pts[[r]],pts[[i]])
                                     ))
                         })
              object@x <- sapply(pts, function(p) p[1])
              object@y <- sapply(pts, function(p) p[2])
              .geometry_from_xy(object)
          }
          )

######################################################################
###
### get sides

safeSetGeneric(name = "sides",
               def = function(object) standardGeneric("sides") )

#' @rdname triangle-class
#' @aliases sides, triangle-method sides
#' @export
setMethod(f = "sides", signature = "triangle",
          definition=function(object)
          {
              object@sides
          }
          )

######################################################################
###
### get angles

safeSetGeneric(name = "angles",
               def = function(object) standardGeneric("angles") )

#' @rdname triangle-class
#' @aliases angles,triangle-method angles
#'
#' @export
setMethod(f = "angles", signature = "triangle",
          definition=function(object) {
              object@angles*if(object@degrees) 180/pi else 1
          })


######################################################################
###
### position triangle in space

safeSetGeneric(name = "place",
               def = function(object, ...) standardGeneric("place") )

#' @rdname triangle-class
#' @aliases place, triangle-method place
#'
#' @export
setMethod(f = "place", signature = c("triangle"),
          definition=function(object, angle=0, origin=c(0,0), ...) {
              # place at initial position
              object@x <- c(0, object@sides[3], object@sides[2]*cos(object@angles[1]))
              object@y <- c(0, 0, object@sides[2]*sin(object@angles[1]))
              # apply transformation
              transform2D(object, translate2D(x=origin[1],
                                              y=origin[2]) %*%
                                  rotate2D(-angle * if(object@degrees) pi/180 else 1))
          })


######################################################################
###
### apply 2D affine transformation

safeSetGeneric(name = "transform2D",
               def = function(object, ...) standardGeneric("transform2D") )

#' @rdname triangle-class
#' @aliases transform2D, triangle-method transform2D
#' @export
setMethod(f = "transform2D", signature = c("triangle"),
          definition=function(object, m = identity2D(), ...) {
              .geometry_from_xy(object,
                                x = apply2D(m,
                                            x = object@x,
                                            y = object@y))
          })


######################################################################
###
### circumference

safeSetGeneric(name = "circumference",
               def = function(object, ...) standardGeneric("circumference") )

#' @rdname triangle-class
#' @aliases circumference, triangle-method circumference
#' @export
setMethod(f = "circumference", signature = c("triangle"),
          definition=function(object) {
              sum(object@sides)
          })

######################################################################
###
### incircle

safeSetGeneric(name = "incircle",
               def = function(object, ...) standardGeneric("incircle") )

#' @rdname triangle-class
#' @aliases incircle, triangle-method incircle
#' @export
setMethod(f = "incircle", signature = c("triangle"),
          definition=function(object) {
              u <- sum(object@sides)
              ri <- sqrt(prod(u/2-object@sides)/(u/2))
              list(x=sum(object@sides/u*object@x),
                   y=sum(object@sides/u*object@y),
                   r=ri)
          })

######################################################################
###
### circumcircle

safeSetGeneric(name = "circumcircle",
               def = function(object, ...) standardGeneric("circumcircle") )

#' @rdname triangle-class
#' @aliases circumcircle, triangle-method circumcircle
#' @export
setMethod(f = "circumcircle", signature = c("triangle"),
          definition=function(object) {
              i <- sapply(1:3, function(i) (i+3:4)%%3+1)
              d <- 2*sum(sapply(1:3, function(j) object@x[j]*diff(object@y[i[,j]])))
              dx <- apply(i, 2, function(j) diff(object@x[j]))
              dy <- apply(i, 2, function(j) diff(object@y[j]))
              sq <- object@x^2+object@y^2
              list(x=sum(sq*dy)/d,
                   y=sum(sq*dx)/d,
                   r=mean(object@sides/2/sin(object@angles)))
          })

######################################################################
###
### area

safeSetGeneric(name = "area",
               def = function(object, ...) standardGeneric("area") )

#' @rdname triangle-class
#' @aliases area, triangle-method area
#' @export
setMethod(f = "area", signature = c("triangle"),
          definition=function(object) {
              sqrt(prod(circumference(object)/2-c(0,object@sides)))
          })


#' @rdname triangle-class
#' @aliases as.data.frame, triangle-method as.data.frame
#' @export
setMethod(f = "as.data.frame", signature = c("triangle"),
          definition=function(x, row.names=letters[1:3], optional=FALSE, ...) {
              tmp <- data.frame(x=x@x, y=x@y,
                         sides=x@sides,
                         angles=x@angles * if(x@degrees) 180/pi else 1)
              row.names(tmp) <- row.names
              tmp
          })


#' @rdname triangle-class
#' @aliases plot, triangle-method plot
#' @export
setMethod(f="plot",
          signature=c("triangle", "missing"),
          definition=function(x,
                              which = c(1L:3L),
                              add=FALSE,
                              ...)
          {
              show <- rep(FALSE, 3)
              show[which] <- TRUE

              if(!add)
                  plot(x@x, x@y, asp=1, ...)
              if(show[1])
                  polygon(x@x, x@y, ...)
              if(show[2]) {
                  ic <- incircle(x)
                  circle2D(gfx2D(), x=ic$x, y=ic$y, r=ic$r, ...)
              }
              if(show[3]) {
                  cc <- circumcircle(x)
                  circle2D(gfx2D(), x=cc$x, y=cc$y, r=cc$r, n=32, ...)
              }
          })


######################################################################
###
### Helper functions for triangle stuff


## calculate the three angles from the sides
.angles_from_sides <- function(sides)
    sapply(1:3,
           function(i)
               acos(sum(ifelse(1:3==i, -1, 1) * sides^2) /
                    (2 * prod(sides[1:3!=i]))))

## initialize sides and angles from x and y coordinates
.geometry_from_xy <- function(tri, x=NULL, y=NULL)
{
    if(!is.null(x)) {
        xy <- xy.coords(x,y)
        tri@x <- xy$x
        tri@y <- xy$y
    }
    tri@sides <- rev(as.numeric(dist(cbind(tri@x,tri@y))))
    tri@angles <- .angles_from_sides(tri@sides)
    tri
}

## unit vector in direction A to B
.unitvec <- function(A, B)
{
    (B-A)/vector.norm(B-A)
}
