#' Return the current date and time, nicely formatted
#'
#' Convenience function that returns date and time formatted as I like them
#' 
#' @param fmt A string specifying the format of date and time. Defaults to \code{\%d-\%b-\%Y \%H:\%M:\%S}.
#'          Check \code{\link{strftime}} for format specifications.
#' @return Current data and time as character string, formatted as specified
#' @examples
#' current_time()
#' @seealso \code{\link{Sys.time}}, \code{\link{strftime}}
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
current_time <- function(fmt="%d-%b-%Y %H:%M:%S") { 
  format(Sys.time(), fmt) 
}
