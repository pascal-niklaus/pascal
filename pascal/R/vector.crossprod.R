#' Vector cross product
#'
#' Calculate the vector cross-product of two 3-D vectors.
#'
#' This function calculates the cross product of two vectors in
#' three-dimensional Euclidean space.  The output vector is
#' perpendicular to the input vectors \code{a} and \code{b} and has a
#' magnitude equal to the area of the parallelogram spanned by
#' \code{a} and \code{b}.
#'
#' Note that \code{crossprod} from package \code{base} calculates the
#' matrix cross-product, which is different.
#' 
#' @param a first 3D-input vector 
#' @param b second 3D-input vector 
#' @return cross-product of \code{a x b}
#' @examples
#' a <- c(1,0,0)
#' b <- c(0,1,0)
#' vector.crossprod(a,b)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords misc, utilities
#' @export    
vector.crossprod <- function(a, b) {
    if(length(a)!=3 || length(b)!=3)
        stop("vectors must have length 3");
    return (a[.vector.crossprod.i1]*b[.vector.crossprod.i2] - a[.vector.crossprod.i2]*b[.vector.crossprod.i1])
}

.vector.crossprod.i1 <- c(2,3,1)
.vector.crossprod.i2 <- c(3,1,2)
