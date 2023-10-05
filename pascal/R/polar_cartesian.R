#' Convert between cartesian and polar coordinates
#'
#' \code{pol2cart} and \code{cart2pol} interconvert between the two
#' coordinate formates. Angles can be provided as radians (default) or
#' in degrees (\code{deg = TRUE}).
#'
#' @param r,phi Polar coordinates. The first argument (\code{r}) may
#'     also be a \code{data.frame}. If this \code{data.frame} contains
#'     columns named \code{"r"} and \code{"phi"} (or alternative names
#'     provided using argument \code{cols=...}, then these are
#'     used. Otherwise, the first and second column in the
#'     \code{data.frame} are assumed to contain \code{r} and
#'     \code{phi}, respectively.
#'#'
#' @param cols Names of columns in \code{data.frame} that contains the
#'     coordinates. If these names are not found, then the 1st and 2nd
#'     column of the \code{data.frame} are used.
#'
#' @param deg logical indicating whether angles are passed as radians
#'     (default) or degrees \code{deg = FALSE}.
#'
#' @return Data frame with polar or cartesian coordinates.
#'
#' @rdname polar_to_cartesian
#'
#' @examples
#' pol2cart(2, 270, deg=TRUE)
#'
#'
#'
#' @export
pol2cart <- function(r, phi = NULL, deg = FALSE, cols = c("r", "phi")) {
    if (is.null(phi)) {
        idx <- match(cols, names(r))
        if (any(is.na(idx))) idx <- c(1, 2)
        phi <- r[[idx[2]]]
        r <- r[[idx[1]]]
    }
    if (deg) phi <- phi / 180 * pi
    data.frame(x = r * cos(phi), y = r * sin(phi))
}

#' @rdname polar_to_cartesian
#'
#' @param x,y Cartesian coordinates. The first argument (\code{x}) may
#'     also be a \code{data.frame}. If this \code{data.frame} contains
#'     columns named \code{"x"} and \code{"y"} (or alternative names
#'     provided using argument \code{cols=...}, then these are
#'     used. Otherwise, the first and second column in the
#'     \code{data.frame} are assumed to contain \code{x} and
#'     \code{y}, respectively.
#'
#'@export
cart2pol <- function(x, y = NULL, deg = FALSE, cols = c("x", "y")) {
    if (is.null(y)) {
        idx <- match(cols, names(x))
        if (any(is.na(idx))) idx <- c(1, 2)
        y <- x[[idx[2]]]
        x <- x[[idx[1]]]
    }
    data.frame(
        r = sqrt(x^2 + y^2),
        phi = (atan2(y, x) %% (2 * pi)) * if (deg) 180 / pi else 1
    )
}
