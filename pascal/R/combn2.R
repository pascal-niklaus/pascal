#' Combinations of n elements, taken m at a Time, with optional replacement
#' 
#' This function is similar to \code{combn2} but allows for
#' combinations resulting from drawing with replacement.
#'
#' To keep implementation simple, \code{combn2} internally uses
#' recursion and is slower than \code{combn}, but this should not be a
#' problem for `reasonably' sized data. Recursion depth is m-2.
#' 
#' @param x vector with data from which combinations are created
#' @param m number of elements to choose#'                
#' @param replace logical indicating whether combinations are created
#' by drawing with replacement, i.e. whether duplicate elements are
#' allowed
#' @param FUN optional function to be applied to combinations
#' @return Matrix with combinations in columns, or result of FUN
#' applied to columns if FUN is provided
#'
#' @seealso \code{\link{combn}} 
#' @examples
#' combn2(LETTERS[1:3],2) 
#' ##     [,1] [,2] [,3]
#' ## [1,] "A"  "A"  "B" 
#' ## [2,] "B"  "C"  "C"
#' 
#' combn2(LETTERS[1:3],2,replace = TRUE)
#' ##     [,1] [,2] [,3] [,4] [,5] [,6]
#' ## [1,] "A"  "A"  "A"  "B"  "B"  "C" 
#' ## [2,] "A"  "B"  "C"  "B"  "C"  "C"
#' 
#' combn2(LETTERS[1:3],2,replace = TRUE,FUN=function(x) paste(x,collapse=":"))
#' ## [1] "A:A" "A:B" "A:C" "B:B" "B:C" "C:C"
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
combn2 <- function(x, m, replace=FALSE, FUN=NULL) {    
    v <- seq_along(x)
    mx <- length(v)
    res <- matrix(NA,
                  ncol=if(replace) choose(mx+m-1,m) else choose(mx,m),
                  nrow=m)
    i <- 0
    
    comb.helper <- function(ch,n) {
        fst <- max( tail(ch,1) ) + if(replace) 0 else 1
        if(fst <= mx) {
            re <- fst:mx
            if(n) {
                for(r in re)
                    comb.helper(c(ch,r),n-1)
            } else
                for(r in re) 
                    res[ , i <<- i + 1 ] <<- c(ch,r)                
        }
    }
    
    if(m == 1)
        res[1,] <- v
    else      
        for(ch in if(replace) v else head(v,-1))
            comb.helper(ch,m-2)
    res <- apply(res,1:2,function(i) x[i])
    if(is.null(FUN))
        res
    else
        apply(res,2,FUN)       
}
