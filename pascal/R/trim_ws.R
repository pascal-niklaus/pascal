#' Trims and white space at ends of string
#'
#' Removes whitespace and tabulator characters at end of strings
#'
#' @param x character vector with data to trim
#' @return character vector with trimmed data
#' @examples
#' trim.ws("  hello!  \t")
#' ## [1] "hello!"
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
trim.ws <- function(x) { 
   gsub("^[\ \t]*", "", gsub("[\ \t]*$", "", x))
}
