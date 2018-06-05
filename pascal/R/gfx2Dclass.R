#' @rdname gfx2D-class
#' @title gfx2D class and initialization
#'
#' The class \code{gfx2D} is used for plotting, applying to all
#' drawing operations a geometric (2D-affine) transformation that is
#' stored as $3 x 3$ matrix.
#'
#' The function \code{gfx2D} creates an object of this class and
#' initializes its transformation matrix. The transformation
#' operations can be passed as arguments (scaling: \code{sx},
#' \code{sy}; rotation: \code{rot}; translation: \code{tx},
#' \code{ty}).
#'
#' transformation (get, set)
#'
#' drawing operations described separately in ?plot2D
#'
#' @examples
#'
#' g <- gfx2D()
#'
#' transformation(g) <- rotate2D(pi/4)
#' 
#' @seealso plot2D,affine2D
#'
#'
#' @import methods
#' @importFrom grDevices xy.coords
#' @name gfx2D-class
#' @exportClass gfx2D
setClass("gfx2D",
         slots = c(m = "matrix", sx = "numeric", sy = "numeric", rot = "numeric", tx = "numeric", ty = "numeric"),
         prototype = prototype( m = diag(nrow=3), sx = 1, sy = 1, rot = 0, tx = 0, ty = 0),
         validity = function(object) return(TRUE))

#' @param tx,ty translation, given either as 2-element vector
#'     \code{tx}, or separately as \code{tx} and \code{ty}.
#' @param sx,sy scaling in x and y direction,  given either as 2-element vector
#'     \code{sx}, or separately as \code{sx} and \code{sy}.
#' @param rot rotation, in radians.
#'
#' @return gfx2D-object
#' 
#' @rdname gfx2D-class
#' @export
gfx2D <- function(tx=c(0,0), ty = tx[2], sx=c(1,1), sy = sx[2], rot = 0)
{    
    g <- new("gfx2D")
    transformation(g) <- scale2D(sx[1],sy[1]) %*% translate2D(tx[1],ty[1]) %*% rotate2D(rot)
    invisible(g)
}

######################################################################
###
### show gfx2D object

#' \code{show(object)} displays the transformation matrix and possibly
#' other options associated with the \code{gfx2D} object.
#'
#' @rdname gfx2D-class
#' @aliases show,gfx2D-method
#' @export
setMethod(f="show",signature="gfx2D",
          definition=function(object) {
              cat("gfx2D\n")
              cat("  Transformation matrix:\n")
              print(transformation(object))
              invisible(NULL)
          })

######################################################################
###
### get and set transformation matrix

safeSetGeneric(name = "transformation<-",
               def = function(object, value) standardGeneric("transformation<-") )

#' @rdname gfx2D-class
#' @aliases transformation<-,gfx2D-method transformation<-
#' @param object gfx2D object
#' @param value transformation matrix
#'
#' \code{transformation(object)} displays the transformation matrix
#' associated with the \code{gfx2D}-object. The transformation matrix
#' can be set with \code{transformation(object) <- m}, where code{m}
#' is a 3 x 3 matrix.  The scaling, rotation, and translation embodied
#' in \code{m} are extracted and stored separately in the object.
#' 
#' @export
setMethod(f = "transformation<-", signature = c("gfx2D"),
          definition=function(object, value) 
          {
              object@m <- value
              object@sx <- sign(value[1,1])*sqrt(value[1,1]^2+value[1,2]^2)
              object@sy <- sign(value[2,2])*sqrt(value[2,1]^2+value[2,2]^2)
              object@rot <- atan2(value[1,2], value[1,1] )
              object@tx <- value[1,3]
              object@ty <- value[2,3]
              invisible(object)
          }
          )

safeSetGeneric(name = "transformation",
               def = function(object) standardGeneric("transformation") )

#' @rdname gfx2D-class
#' @aliases transformation,gfx2D-method transformation
#'
#' @export
setMethod(f = "transformation", signature = "gfx2D",
          definition=function(object)
          {
              tmp <- object@m
              attr(tmp,"sx") <- object@sx
              attr(tmp,"sy") <- object@sy
              attr(tmp,"rot") <- object@rot
              attr(tmp,"tx") <- object@tx
              attr(tmp,"ty") <- object@ty
              tmp
          }
          )



