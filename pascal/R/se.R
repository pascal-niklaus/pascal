#' Standard error of a sample
#'
#' This function computes the standard deviation of the mean of the values in \code{x}.  
#' If \code{na.rm} is \code{TRUE}, missing values are removed before computation proceeds.
#'
#' This convenience function returns \code{sd} divided by the square root
#' of the number of data points.
#' @param x vector of data
#' @param na.rm logical. Should missing values be removed?
#' @return Standard error of sample
#' @seealso \code{\link{sd}} 
#' @examples
#' sd(1:5)
#' ## [1] 1.581139
#' se(1:5)
#' ## [1] 0.7071068
#' se(c(1:5,NA))
#' ## [1] NA
#' se(c(1:5,NA),na.rm=TRUE)
#' ## [1] 0.7071068
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
#' @keywords utilities, misc
se <- function(x,na.rm=FALSE) {
  if(na.rm)
    return(sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)])))
  else
    return(sd(x)/sqrt(length(x))); 
}
