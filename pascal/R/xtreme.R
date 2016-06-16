#' Robust detection of extreme values
#' 
#' Detect extreme values based on interval defined by median absolute deviation
#' 
#' The function \code{xtreme} checks which values are more than \code{f} median absolute deviations  
#' (scaled to standard deviation equivalents) away from the median.  
#' With the default value of f = 3.5 and a perfect normal distribution, only 1 in ca. 2000 
#' values would be classified as extremes. With f = 4, the fraction of extremes reduces to 1 in 16000 values.   
#' 
#' @param x vector of data
#' @param f number of sd-equivalents defining extreme values
#' @keywords misc utilities
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @seealso \code{\link{mad}}
#' @examples
#' pnorm(-3.5)*2; # fraction of data more than 3.5 sd away from mean
#' y <- rnorm(1e6)
#' sum(xtreme(y))/1e6;
#' @importFrom stats mad median 
#' @export
xtreme <- function(x,f=3.5) 
{ 
  tmp<-abs(x-median(x,na.rm=TRUE))>f*mad(x,na.rm=TRUE); 
  !is.na(tmp) & tmp; 
}
