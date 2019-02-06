#' Cotangent
#'
#' \code{cot} calculates the cotangent. It is better than computing
#' 1/tan(x) like done in \code{pracma} because it does not fail for
#' \code{pi/2}.
#'
#' @param z argument in radians, or multiple of pi
#' @return cotangent of z
#'
#' @rdname cot
#' @examples
#' cot(0.25*pi)
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
cot <- function(z)
    tan(0.5*pi-z)

#' @rdname cot
#' @export
cotpi <- function(z)
    cotpi(0.5-z)
