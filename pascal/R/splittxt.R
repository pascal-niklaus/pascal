#' Split (wrap) text strings to given width
#' 
#' \code{splittxt} splits a given text string into parts of a given length,
#' ignoring word boundaries, whitespace etc. It was originally written to
#' wrap DNA sequences to a certain line width.
#'
#' The parameter \code{n} indicated the width of the output. If
#' \code{x} has length > 1 (e.g. is a list), then a list is returned
#' with each element of \code{x} split separately.
#'
#' @param x Text string to wrap. Can also be a list or vector, in
#'     which case the result is a list of vectors with the wrapped
#'     parts.
#'
#' @param n Width of the result in characters (defaults to 80)
#'
#' @return Vector with text split into parts of size n, or a list of
#'     vectors if several arguments were passed.
#'
#' @examples
#' splittxt(paste(LETTERS[1:26],collapse=""),10)
#' 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
splittxt <- function(x, n = 80) {
    x <- as.character(x)
    if (length(x) > 1) {
            lapply(x, splittxt)
    } else {
        i0 <- seq(1, max(1, nchar(x)), n)
        i1 <- seq(n, max(n, nchar(x) + n - 1), n)
        substring(x, i0, i1)
    }
}
