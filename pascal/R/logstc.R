#' Logistic function
#'
#' This is a three-parameter sigmoidal (logistic) function.
#'
#' The function is defined as \code{f(x,a,b,tau)=a*(1-1/[1+exp(tau*(t-b))])}.
#'
#' @param x points where the function is to be evaluated
#' @param a scaling factor, i.e. maximum value the function reaches
#' @param b x coordinate of point of inflection 
#' @param tau coefficient indicating the growth in the exponential part of the curve, 
#'            i.e. the curve approximates \code{exp(tau*x)}
#' @return function evaluated at locations passes in \code{x}
#' @examples
#' x<-0:100
#' plot(-1,-1,xlim=range(x),ylim=c(0,1),xlab="x",ylab="logstc")
#' for(tau in c(.1,.5,1)) {
#'    lines(x,logstc(x,1,50,tau))
#' }  
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords misc, utilities
#' @export    
logstc <- function(x,a=1,b=1,tau=1) {
  return (a*(1-1/(1+exp(tau*(x-b)))));
}
 