#' p-norm of a vector
#'
#' Calculate the p-norm of a vector, or of a list of vectors
#'
#' This function calculates the p-norm of a vector or a list of
#' vectors, which is \code{(sum_i x_i^p)^(1/p)}. For p=1, this is
#' equivalent to the Manhattan norm, for p=2 to the Euclidean
#' norm.
#'
#' The Euclidean norm of a vector can also be obtained by calling
#' \code{norm(x,"2")} from package \code{base}, but this is much
#' slower.
#'  
#' @param x input vector, or list of vectors 
#' @param p type of norm (default 2) 
#' @return norm of vector, or vector with norms of vectors passed in list
#' @examples
#' a <- c(1,2,3)
#' d <- list(a,c(2,3,4,5,6))
#' vector.norm(a,1)
#' vector.norm(a,2)
#' norm(a,"2")
#' vector.norm(d)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords misc, utilities
#' @seealso norm
#' @export    
vector.norm <- function(x, p=2) {
    if(is.list(x))
        sapply(x,function(x) sum(x^p)^(1/p))
    else
        sum(x^p)^(1/p)
}
