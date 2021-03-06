#' Analysis of variance, preserving the specified order of factors in the model formula
#'
#' \code{aov} generally re-orders factors, with higher-order interactions fitted last.
#' \code{aov.ko} calls \code{aov}, specifying an option that prevents this.
#' 
#' Factor reordering sometimes is undesired. In particular, if one analyses a hierarchical data set
#' and wants to specify error terms as fixed effects, and then calculate the correct
#' F-tests manually (or using \code{\link{aov.ftest}}), factor orders need to be preserved.
#' While it is possible to write \code{terms(..., keep.order=TRUE)}, this is cumbersome
#' and reduced readability.
#' \code{aov.ko} calls \code{aov}, applying this option.
#' 
#' @param formula see \code{aov} for details
#' @param data see \code{aov} for details
#' @param projections see \code{aov} for details
#' @param qr see \code{aov} for details
#' @param contrasts see \code{aov} for details
#' @param ... see \code{aov} for details
#' @return see \code{aov} for details
#' @examples
#' \dontrun{
#' library(pascal)
#' require(MASS)
#' utils::data(oats, package="MASS")
#' oats.aov <- aov(Y ~ B+N*V + Error(B:V), data = oats)
#' summary(oats.aov)
#' ## Error: B:V
#' ##           Df Sum Sq Mean Sq F value Pr(>F)  
#' ## B          5  15875    3175    5.28  0.012 *
#' ## V          2   1786     893    1.49  0.272  
#' ## Residuals 10   6013     601                 
#' ## 
#' ## Error: Within
#' ##           Df Sum Sq Mean Sq F value  Pr(>F)    
#' ## N          3  20020    6673    37.7 2.5e-12 ***
#' ## N:V        6    322      54     0.3    0.93    
#' ## Residuals 45   7969     177  
#'
#' oats.aov <- aov.ko(Y~B+V+B:V+N+N:V,data=oats)
#'
#' ## In the output that follows, the test for V is pseudoreplicated!
#' summary(oats.aov)
#' ##             Df Sum Sq Mean Sq F value  Pr(>F)    
#' ## B            5  15875    3175   17.93 9.5e-10 ***
#' ## V            2   1786     893    5.04  0.0106 *   
#' ## B:V         10   6013     601    3.40  0.0023 ** 
#' ## N            3  20020    6673   37.69 2.5e-12 ***
#' ## V:N          6    322      54    0.30  0.9322    
#' ## Residuals   45   7969     177
#'
#' ## Now we calculate the properly replicated test:
#' aov.ftest(oats.aov,V~B:V,table=TRUE)
#' ##   nom den df ddf      SS     MS    F     P s
#' ## 1   V B:V  2  10 1786.36 893.18 1.49 0.273     
#' }
#' @seealso \code{\link{aov}}
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
aov.ko <- function (formula, data = NULL, projections = FALSE, qr = TRUE, contrasts = NULL, ...)
{
  m <- match.call();
  m[[1]]<-as.name("aov");
  m$formula <- terms(formula,keep.order=TRUE);  
  eval(m,parent.frame());    
}
