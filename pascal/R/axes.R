#' Plot axis labels
#'
#' \code{left.axis}, \code{bottom.axis}, \code{right.axis} and
#' \code{top.axis} simply plot the corresponding axes.
#'
#' I always forget the numbering of the different axes, and therefore
#' introduced these convenience functions which simply pass through
#' all parameters to the base \code{axis} commend (but add the correct
#' axis number). A further difference is that by default (if
#' \code{las} is not specified), all labels are printed horizontally,
#' which I prefer for publication-quality figures.
#'
#' @param ... parameter passed to \code{axis}. See \code{?axis} for
#'     details.
#'
#' @examples
#' \dontrun{
#' ## silly example...
#' plot(rnorm(10), rnorm(10), xlim=c(-3,3), ylim=c(-3,3),
#'      xaxt="n", yaxt="n", xaxs="i", yaxs="i")
#' xvec <- seq(-2, 2, by=2)
#' left.axis(at=xvec)
#' bottom.axis(at=xvec)
#' }
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname axis
#' @importFrom graphics axis
#' @export
bottom.axis <- function(...) {
    axis(1, ...)
}

#' @rdname axis
#' @export
left.axis <- function(...) {
    x <- list(...)
    if ("las" %in% names(x))
        axis(2, ...)
    else
        axis(2, ..., las=1)
}

#' @rdname axis
#' @export
top.axis <- function(...) {
    axis(3, ...)
}

#' @rdname axis
#' @export
right.axis <- function(...) {
    x <- list(...)
    if ("las" %in% names(x))
        axis(4, ...)
    else
        axis(4, ..., las=1)
}
