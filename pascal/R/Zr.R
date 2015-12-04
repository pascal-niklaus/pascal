#' Calculate normalised effect sizes 
#'
#' In meta-analyses, one often uses Z-transformed effect sizes.  This
#' function returns such effect sizes, computed from F values and
#' associated nominator and denominator degrees of freedom.
#'  
#' @param F F-ratio (e.g. from ANOVA table)
#' @param df nominator degrees of freedom (default: 1)
#' @param dendf denominator degrees of freedom
#'             
#' @return Fisher's Z-transformed effect size
#' @examples
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
