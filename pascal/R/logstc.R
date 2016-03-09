#' Logistic function
#'
#' Three-parameter sigmoidal (logistic) function and derivatives
#'
#' The function is defined as \code{f(x,a,b,tau)=a*(1-1/[1+exp(tau*(x-b))])}.
#' Derivatives are calculated analytically.
#'
#' @param x points where the function is to be evaluated
#' @param a scaling factor, i.e. maximum value the function reaches
#' @param b x coordinate of point of inflection 
#' @param tau coefficient indicating the growth in the exponential part of the curve, 
#'            i.e. the curve approximates \code{exp(tau*x)}
#' @return function evaluated at locations passed in \code{x}
#' @examples
#' x<-seq(40,60,len=100)
#' plot(-1,-1,xlim=range(x),ylim=c(-.1,1),xlab="x",ylab="logstc")
#' for(tau in c(.5,1)) {
#'     lines(x,logstc(x,1,50,tau),col="red")
#'     lines(x,logstc.der(x,1,50,tau),col="blue")
#'     lines(x,logstc.der2(x,1,50,tau),col="green")    
#'  }  
#' legend("topleft",c("function","1st derivative","2nd derivative"),lty=1,col=c("red","blue","green"))
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords misc, utilities
#' @describeIn logstc logistic function
#' @export    
logstc <- function(x,a=1,b=1,tau=1) {
  return (a*(1-1/(1+exp(tau*(x-b)))));
}

#' @describeIn logstc 1st derivative of logistic function
#' @export
logstc.der <- function(x,a=1,b=1,tau=1) {
    tmp <- exp(tau*(x-b))
    return ( a * tau * tmp *(1+tmp)^-2 )
}

#' @describeIn logstc 2nd derivative of logistic function
#' @export
logstc.der2 <- function(x,a=1,b=1,tau=1) {
    tmp <- exp(tau*(x-b))
    return ( -a * tau * tau * tmp * (tmp - 1) * ( tmp + 1 )^-3 )
}


