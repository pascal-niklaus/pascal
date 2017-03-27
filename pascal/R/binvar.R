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
#' @param frac amount of data in bin. \code{frac} is normalized to
#'     unity sum. Note that \code{frac} is not the probability
#'     density, which would be \code{frac/width}.
#'
#' @param width width of the bin
#'
#' @return variance of distribution
#' 
#' @examples
#' ## simulate data in two bins:
#' xsim <- c(runif(10000,0,1),runif(30000,1,1.5))
#' hist(xsim,breaks=seq(0,2,by=.1))
#' x <- c(.5, 1.25) # center of bins
#' w <- c(1, .5) # width of bins
#' f <- c(1,3)   # amount of data in bins
#' binvar(x,f,w)
#' var(xsim) # compare to simulated data
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @importFrom stats weighted.mean 
#' @keywords misc, utilities
#' @export
binvar <- function(x, frac, width = 1) {
    if(length(width) == 1)
        width <- rep(width,length(x))    
    frac <- frac / sum(frac)
    xbar <- weighted.mean(x,frac)
    intf <- function(x) {
        x^3/3 - xbar*x^2 + xbar^2 * x
    }            
    sum( frac / width * ( intf( x + width/2 ) - intf( x - width/2 ) ))
}
