#' safeSample
#'
#' This function can replace \code{base::sample}, which is ambiguous
#' for vector lengths of 1 in case the type is numeric.
#'
#' \code{sample} is IMO unfortunate, because the behaviour of
#' \code{sample} becomes identical to the one of \code{sample.int}
#' when the argument \code{x} is of length 1 and of type numeric.
#'
#' @param x a vector of one or more elements from which to
#'     choose. Unlike \code{base::sample}, \code{x} is also
#'     interpreted as vector if it is of numeric type.
#'
#' @param size a non-negative integer giving the number of items to
#'     choose.
#'
#' @param replace should sampling be with replacement?
#'
#' @param prob a vector of probability weights for obtaining the
#'     elements of the vector being sampled.
#'
#' @seealso \code{\link{sample}}
#'
#' @examples
#' ## these are the same:
#' x <- 1:10
#' sample(x, 1)
#' safeSample(x, 1)
#'
#' ## these are different
#' x <- c(10)       # a vector of length 1
#' sample(x, 1)     # will return the same as sample(1:10, 1)
#' safeSample(x, 1) # will return the value 10
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
safeSample <- function(x, size, replace = FALSE, prob = NULL) {
    if (length(x) > 1) {
        base::sample(x, size, replace, prob)
    } else if (size == 1 || replace) {
        rep(x, size)
    } else {
        stop("Error in safeSample: size>n and replace==FALSE")
    }
}
