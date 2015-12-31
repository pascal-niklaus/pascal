#' Remove factors property from data frame columns
#' 
#' \code{killfactors} converts factors in data frames to regular
#' numeric of character vectors.  R automatically converts strings to
#' factors when data frames are created. This is not always desired,
#' and \code{killFactors} will ``undo'' this behaviour.
#' 
#' @param d Source data frame containing the data set to aggregate
#' 
#' @param restore.numerics Logical determining whether numeric columns
#'     are converted to numeric instead of the default character type
#'     (default FALSE). A column is considered numeric when all values
#'     except NA can be converted to valid numeric data including NaN
#'     and Inf.
#'
#' @param cols Vector determining which columns should be inspected
#'     (default: all). Columns can be specified by index or name.
#' 
#' @examples
#' d <- data.frame(c1=LETTERS[1:5],c2=factor(1:5),c3=1:5)
#' str(d)
#' ## 'data.frame':	5 obs. of  3 variables:
#' ##  $ c1: Factor w/ 5 levels "A","B","C","D",..: 1 2 3 4 5
#' ##  $ c2: Factor w/ 5 levels "1","2","3","4",..: 1 2 3 4 5
#' ##  $ c3: int  1 2 3 4 5
#' str(killfactors(d))
#' ## 'data.frame':	5 obs. of  3 variables:
#' ##  $ c1: chr  "A" "B" "C" "D" ...
#' ##  $ c2: chr  "1" "2" "3" "4" ...
#' ##  $ c3: int  1 2 3 4 5
#' str(killfactors(d,restore.numerics = TRUE))
#' ## 'data.frame':	5 obs. of  3 variables:
#' ##  $ c1: chr  "A" "B" "C" "D" ...
#' ##  $ c2: num  1 2 3 4 5
#' ##  $ c3: int  1 2 3 4 5
#' 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
killfactors <- function(d, restore.numerics = FALSE, cols= seq_along(names(d))) {
    if(is.character(cols)) {
        cols <- match(cols,names(d))
        if(any(is.na(cols))) {
            stop("Column not found in data frame")
        }
    }
    for(i in cols) {
        if(is.factor(d[,i])) {
            if(restore.numerics) {
                tmp <- suppressWarnings(as.numeric(as.character(d[,i])))
                if( all(is.na(d[,i]) == ( is.na(tmp) & !is.nan(tmp) ) ) ) 
                    d[,i] <- tmp
                else
                    d[,i] <- as.character(d[,i])
            } else {
                d[,i] <- as.character(d[,i])
            }
        }        
    }
    d    
}
