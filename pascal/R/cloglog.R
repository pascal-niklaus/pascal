
#' Complementary log-log link function and its inverse
#'
#' \code{cloglog} will return the complementary log log function, whereas
#' \code{invcloglog} will invert it.
#' \code{cloglog} will yield -Inf and Inf for arguments 0 and 1, or NaN for values
#' outside this range. Conversely, \code{invcloglog} handles inputs of -Inf and Inf.
#' 
#' @param x A vector, list or matrix containing character or numeric data, possibly as factor.
#' @return A numeric vector, list or matrix containing the converted data.
#'
#'
#' @examples
#' library(pascal)
#' x <- factor(3:5)
#' x
#' ## [1] 3 4 5
#' ## Levels: 3 4 5
#' as.numeric(x)
#' ## [1] 1 2 3
#' safen(x)
#' ## [1] 3 4 5
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname cloglog
#' @export
cloglog <- function(p) {
    ifelse(p<0 || p>1, NaN, log(-log(1-p)))
}

#' @rdname cloglog
#' @export
invcloglog <- function(eta)
{
    1- exp(-exp(eta))
}

