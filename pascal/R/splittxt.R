#' Split (wrap) text strings to given width
#' 
#' \code{splittxt} splits a given text string into parts of a given length,
#' ignoring word boundaries, whitespace etc. It was originally written to
#' wrap DNA sequences to a certain line width.
#'
#' The \code{factors} defining the grouping are given as character
#' string and can be given new names in the aggregated data set. For
#' example, \code{factors=c("f1=factor1","f2=factor2")} will rename
#' the factors \code{factor1} and \code{factor2} to \code{f1} and
#' \code{f2}.  If no new name is given, the original is kept.
#'
#' The summary column defined in \code{newcols} is passed as character
#' vector with the form
#' \code{new_name=summary_function(old_name)}. Any defined function
#' taking a numeric vector as argument can be used.  For example,
#' \code{newcols=c("m.tot=sum(m)","m.avg=mean(m)","n=length(m)")} will
#' compute the sum, average and number of data in \code{m} for all
#' factor combinations and return them under the names \code{m.tot},
#' \code{m.avg} and \code{n}.  The R code given is not fully parsed;
#' instead, any name enclosed in parenthesis (without whitespace) is
#' treated as column name (textual replacement).  This allows for
#' terms such as \code{newcols=c("m.tot=sum((m),na.rm=T)")}.  As a
#' trade-off, \code{newcols=c("m.tot=sum(( m ),na.rm=T)")} will not
#' work because there is whitespace around the variable name
#' \code{m}. This can be used to distinguish between column names and
#' other variables defined in the scope in which \code{aggr} is
#' executed.
#'
#' If the package \code{parallel} is installed, some computations are
#' parallelized on platforms other than Windows, using all available
#' processor cores.  However, the performance gain is small
#' for simple summary functions.
#' 
#' @param x Text string to wrap. Can also be a list or vector, in which case
#' the result is a list of vectors with the wrapped parts.
#' @param n Width of the result in characters (defaults to 80) 
#' @return Vector with text split into parts of size n, or a list of vectors
#' if several arguments were passed.
#'
#' @examples
#' splittxt(paste(LETTERS[1:26],collapse=""),10)
#' 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
splittxt <- function(x, n=80) {
    x <- as.character(x)
    if(length(x)>1) 
            lapply(x,splittxt)
    else {                
        i0 <- seq(1, max(1,nchar(x)), n)        
        i1 <- seq(n, max(n,nchar(x)+n-1), n)
        substring(x, i0, i1)
    }
}
