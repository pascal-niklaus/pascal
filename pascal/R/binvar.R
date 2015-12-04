#' Variance of distribution of binned data
#'
#' Calculates the population variance of a distribution given by bins
#' of a certain size, assuming an uniform distribution within each bin,
#' i.e. a ``staircase-shaped'' probability density function.
#' Note that the estimated variance will be biased in the case of discretized data,
#' i.e. it will differ from the one of the original population.
#' 
#' @param x vector with the center of the bins
#' 
#' @param rho relative probability density for each bin. \code{rho} is
#'     normalized to unity integral, i.e. \code{sum(width*rho_i) = 1}.
#'
#' @param width width of the bin
#'
#' @return variance of distribution
#' 
#' @examples
#'     -4
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' 
#' @keywords misc, utilities
#' @export
binvar <- function(x, rho, width = 1) {
    rho <- rho / (width*sum(rho))
    xbar <- weighted.mean(x,rho)
    intf <- function(x) {
        x^3/3 - xbar*x^2 + xbar^2 * x
    }            
    sum(
        sapply(
            seq_along(x),
            function(i) {
                rho[i] * ( intf( x[i] + width/2 ) - intf( x[i] - width/2 ) )
            }
        ))
}

