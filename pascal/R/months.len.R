#' Length of months in a regular (non-leap) year
#'
#' Vector of length 12, containing the length of a month in days, for a non-leap year.
#'
#' The length of the months are calculated as follows:
#'
#' \code{as.POSIXlt(seq(as.Date("2007-02-01"), by="month", length.out=12)-1)$mday}
#' 
#' @examples
#' data.frame(month.abb,months.len)  
#' ##    month.abb months.len
#' ## 1        Jan         31
#' ## 2        Feb         28
#' ## 3        Mar         31
#' ## 4        Apr         30
#' ## 5        May         31
#' ## 6        Jun         30
#' ## 7        Jul         31
#' ## 8        Aug         31
#' ## 9        Sep         30
#' ## 10       Oct         31
#' ## 11       Nov         30
#' ## 12       Dec         31
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords utilities, misc
#' @export    
months.len <- as.POSIXlt(seq(as.Date("2007-02-01"), by="month", length.out=12)-1)$mday
