#' Compute ANOVA table with non-standard residual terms
#'
#' Sometimes one wants to calculate F-tests using a non-standard
#' residual term. This functions helps to achieve this. Note
#' that you need to understand exactly what you are doing when
#' using this function, in particular if data sets are unbalanced.
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
#' @importFrom stats pf terms 
#' @examples
#' library(pascal)
#' require(MASS)
#' utils::data(oats, package="MASS")
#' oats.aov <- aov(Y ~ N*V + Error(B/V), data = oats)
#' summary(oats.aov)
#'
#' # Error: B
#' #           Df Sum Sq Mean Sq F value Pr(>F)
#' # Residuals  5  15875    3175               
#' # 
#' # Error: B:V
#' # Df Sum Sq Mean Sq F value Pr(>F)
#' # V          2   1786   893.2   1.485  0.272
#' # Residuals 10   6013   601.3               
#' # 
#' # Error: Within
#' #           Df Sum Sq Mean Sq F value   Pr(>F)    
#' # N          3  20020    6673  37.686 2.46e-12 ***
#' # N:V        6    322      54   0.303    0.932    
#' # Residuals 45   7969     177        
#'
#' oats.aov2 <- aov(terms(Y~B+V+B:V+N+N:V,keep.order=TRUE),data=oats)
#' summary(oats.aov2)
#'
#' ## note: the following table contains pseudoreplicated tests!
#' #             Df Sum Sq Mean Sq F value   Pr(>F)    
#' # B            5  15875    3175  17.930 9.53e-10 ***
#' # V            2   1786     893   5.044  0.01056 *  
#' # B:V         10   6013     601   3.396  0.00225 ** 
#' # N            3  20020    6673  37.686 2.46e-12 ***
#' # V:N          6    322      54   0.303  0.93220    
#' # Residuals   45   7969     177                  
#'
#' aov.ftest(oats.aov2,V~B:V)
#' # This produces an F-test against the proper error stratum (plot = B:V)
#' # [1] 0.2723869
#'
#' # multiple tests can be passed as a list, and the output formatted as a table
#' aov.ftest(oats.aov2,list(V~B:V,N~Residuals,V:N~Residuals),table=TRUE)
#' #   nom       den df ddf       SS      MS     F     P   s
#' # 1   V       B:V  2  10  1786.36  893.18  1.49 0.273    
#' # 2   N Residuals  3  45 20020.50 6673.50 37.69 0.001 ***
#' # 3 V:N Residuals  6  45   321.75   53.62  0.30 0.933    
#'
#' aov.ftest(oats.aov2,list(V~B:V,N~Residuals,V:N~Residuals),table=FALSE)
#' # [1] 2.723869e-01 2.457710e-12 9.321988e-01
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
aov.ftest <- function (aovobj, test.formula, table=FALSE)
{
  trim <- function(x) {  
    sub("^ *(.*?) *$","\\1",x);
  }
  a <- summary(aovobj,intercept=TRUE)[[1]];                        
  rownames(a) <- sorted.code(trim(rownames(a)),split=":");
  if(!is.list(test.formula)) 
    test.formula <- list(test.formula);
  ntests <- length(test.formula);
  r <- data.frame();
  p.values <- rep(NA,ntests);
  for(i in 1:ntests) {
    nom <- as.character(test.formula[[i]])[2];
    den <- as.character(test.formula[[i]])[3];

    nom.row <- which(sorted.code(nom,split=":") == trim(rownames(a)));
    den.row <- which(sorted.code(den,split=":") == rownames(a));

    if(length(nom.row)!=1) stop("Nominator ",nom," not found");
    if(length(den.row)!=1) stop("Denominator ",den," not found");
    
    F <- a[nom.row, "Mean Sq"]/a[den.row, "Mean Sq"]              ## calculate F value from MS
    df1 <- a[nom.row, "Df"]
    df2 <- a[den.row, "Df"]
    p <- pf(F, df1, df2, lower.tail = FALSE)                      ## calculate P-value (F-test)
    s <- ifelse(p<=0.001,"***",ifelse(p<=0.01,"**",ifelse(p<=0.05,"*",ifelse(p<=0.1,".",""))));
    r <- rbind(r,data.frame(nom=trim(nom), den = trim(den), df = df1, ddf = df2, 
                            SS = sprintf("%.2f", a[nom.row, "Sum Sq"]), 
                            MS = sprintf("%.2f", a[nom.row, "Mean Sq"]),  
                            F =  sprintf("%.2f", F), P = sprintf("%.3f", 
                            ceiling(1000*p)/1000), s));
    p.values[i]<-p;
  }
  if(table) {
    if(!is.null(names(test.formula)))
      rownames(r) <- names(test.formula);
      r;
  } else 
    p.values;
}
