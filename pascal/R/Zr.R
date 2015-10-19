#' Calculate normalised effect sizes 
#'
#' Calculates effects sizes from F values and associated denominator
#' degrees of freedom.
#'  
#' @param aovobj An object containing the result of \code{aov}
#' @param test.formula A formula specifying nominator and denominator of the test.
#'               Several formula can be passed as a list.
#' @param table TRUE indicates that the results should be returned as a formatted table, 
#'              whereas FALSE will return a vector of P values.
#' @return A vector of P-values if \code{table=FALSE}, or a \code{data.frame} containing
#' a formatted ANOVA table if \code{table=TRUE}. Note that all P values are rounded
#' up to the next displayed digit, i.e. all P values < 0.001 will be indicated as 0.001.
#' @seealso \code{\link{aov}} 
#' @examples
#' library(pascal)
#'
#' Zr(4.5,10)
#' 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
Zr <- function (F, dendf, df=1)
{
    r <- sqrt(df*F / (df*F + dendf ) )
    0.5 * log((1+r)/(1-r))
}
