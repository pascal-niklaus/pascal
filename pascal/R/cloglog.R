#' Complementary log-log link function and its inverse
#'
#' \code{cloglog} will return the complementary log log function, whereas
#' \code{invcloglog} will invert it.
#' \code{cloglog} will yield -Inf and Inf for arguments 0 and 1, or NaN for values
#' outside this range. Conversely, \code{invcloglog} handles inputs of -Inf and Inf.
#' 
#' @param x Numeric vector or matrix containing data to be transformed
#' @return Transformed data
#'
#' @examples
#' cloglog(.3)
#' ## [1] -1.03093
#' 
#' invcloglog(cloglog(.3))
#' ## [1] 0.3
#' 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname cloglog
#' @export
cloglog <- function(x) {
    ifelse(x<0 || x>1, NaN, log(-log(1-x)))
}

#' @rdname cloglog
#' @export
invcloglog <- function(x)
{
    1- exp(-exp(x))
}

