#' Square root with sign reconstruction
#'
#' Essentially, this function is implemented as \code{sign(x)*sqrt(abs(x))}.
#'
#' @param x values passed to the function
#'
#' @return  square root of absolute value of x, with the sign of x reconstructed
#'
#' @examples
#' sgnsqrt(c(16,-16))
#' ## [1]  4 -4
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords misc, utilities
#' @export
sgnsqrt <- function(x) sign(x) * sqrt(abs(x))
