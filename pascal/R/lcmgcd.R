#' Least common multiple 
#'
#' Given two integers, or a vector of integers, determine the least common multiple.
#'
#' This function is not called \code{lcm} because there already is a function of that name
#' in \code{package:graphics}.
#' 
#' @param a integer, or vector of integers in no parameter b is passed
#' @param b integer
#' @return least common multiple
#' 
#' @references This functions are adapted from library \code{pracma}
#'
#' @examples
#' library(pascal)
#' leastCommonMultiple(3,4)
#' leastCommonMultiple(c(3,4,5))
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export  
leastCommonMultiple <- function(a, b=NULL)
{
    if(is.null(b)) {
        x <- unique(a)
        while((n <- length(x)) > 1)
            x <- sapply(seq(1, n, by=2),
                        function(i)
                            if(i==n) x[i] else lcm2(x[i],x[i+1])) 
        x
    } else
        lcm2(a,b)
}

lcm2 <- function (a, b) 
{
    g <- greatestCommonDenominator(a, b)
    a/g * b
}

#' Greatest common denominator
#'
#' Given two integers, determines the greates common denominator.
#' 
#' @param a integer
#' @param b integer
#' @return greatest common denominator
#' 
#' @references This functions are adapted from library \code{pracma}
#'
#' @examples
#' library(pascal)
#' greatestCommonDenominator(15,9)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export  
greatestCommonDenominator <- function (a, b)
{
    e <- d <- g <- 1
    u <- c(1, 0, abs(a))
    v <- c(0, 1, abs(b))
    while (v[3] != 0) {
        q <- floor(u[3]/v[3])
        t <- u - v * q
        u <- v
        v <- t
    }
    u[3]
}

