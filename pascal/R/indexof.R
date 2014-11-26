#' Find column index in data frame
#'
#' Deprecated -- is just there for compatibility with old code
#' and will eventually be removed
#' @param d data frame
#' @param col column name
#' @return indices of columns with the specified names
#' @seealso \code{\link{which}} 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
indexof <- function (d = NULL, col = NULL)
{
    return(sapply(col,function(x) which(names(d) == x)));
}
