#' Displays ANOVA table, stratum variances, and variance components
#'
#' Given an ASReml object, this function displays the corresponding ANOVA table, error strata, and variance components.
#'
#' Wald tests are reported with the \code{default} denominator degrees of freedom option of \code{wald.asreml}, adjusted according to Kenward \& Roger.
#' The function tests for convergence of the model. Stratum variances are reported if available in the summary object \code{asreml} provides.
#'
#' @param d.asr asreml object, returned from calling \code{asreml} or \code{asreml.nvc}
#' @param returnWald logical. If TRUE, \code{test.asreml} will return the table with the wald tests.
#' @param silent logical. If TRUE, no output will be produced. This is useful when capturing the wald tests for further processing.
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @examples
#' \dontrun{
#' library(pascal)
#' library(asreml)
#' data(oats, package="asreml")
#' oats.asr <- asreml(yield~Blocks+Nitrogen*Variety,random=~Blocks:Variety,data=oats)
#' test.asreml(oats.asr)
#' }
#' ## ---- Wald tests:
#' ##                  Df denDF F.inc      Pr    
#' ## (Intercept)       1    10  1294 6.5e-12 ***
#' ## Blocks            5    10     5   0.012 *  
#' ## Nitrogen          3    45    38 2.5e-12 ***
#' ## Variety           2    10     1   0.272    
#' ## Nitrogen:Variety  6    45     0   0.932    
#' ##
#' ## ---- Stratum variances:
#' ##                df Variance Blocks:Variety R!variance
#' ## Blocks:Variety 10    601.3              4          1
#' ## R!variance     45    177.1              0          1
#' ## 
#' ## ---- Variance components:
#' ##                            gamma component std.error z.ratio constraint
#' ## Blocks:Variety!Blocks.var 0.5989     106.1     67.88   1.563   Positive
#' ## R!variance                1.0000     177.1     37.33   4.743   Positive
#' ## 
#' ## ---- Dispersion:
#' ## 13.31 
#' @keywords utilities, misc
#' @seealso wald.asreml 
#' @importFrom stats printCoefmat
#' @importFrom utils capture.output
#' @export
test.asreml <- function(d.asr,returnWald=FALSE,silent=FALSE) 
{
  if(d.asr$converge==FALSE || d.asr$ifault!=0) {
    cat("--- asreml did not coverge properly:\n");
    cat("MSG: ",d.asr$last.message,"\n");
    cat("CALL:\n");  
    print(d.asr$call);
  } else {
    capture.output(tab <- asreml::wald.asreml(d.asr,denDF="default"));
    digits <- max(getOption("digits") - 2, 3) 
    signif.stars = getOption("show.signif.stars")
    nc <- dim(tab$Wald)[2L]
    cn <- colnames(tab$Wald);
    has.P <- grepl("^(P|Pr)", cn[nc])
    zap.i <- 1L:(if (has.P) nc - 1 else nc);
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if (length(i)) 
        zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if (length(i <- grep("Df$", cn))) 
        zap.i <- zap.i[!(zap.i %in% i)]      
        
    if(!silent) {
      cat("\n---- Wald tests:\n");
      printCoefmat(tab$Wald, digits = digits, signif.stars = signif.stars, 
            has.Pvalue = has.P, P.values = has.P, cs.ind = NULL, 
            zap.ind = zap.i, tst.ind = tst.i, na.print = "");        
    
      if(!is.null(tab$stratumVariances)) {
        cat("\n---- Stratum variances:\n");
        print(tab$stratumVariances);
      }
    
      cat("\n---- Variance components:\n");    
      print(summary(d.asr)$varcomp);

      cat("\n---- Dispersion:\n");    
      cat(summary(d.asr)$sigma,"\n");
    }
  }         
  if(returnWald)
    invisible(tab$Wald)
  else
    invisible(d.asr)
}

