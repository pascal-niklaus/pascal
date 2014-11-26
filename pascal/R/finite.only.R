#' Return finite elements of a list or vector
#'
#' Convenience function that removes non-finite elements (i.e. NA, NaN, Inf, -Inf) from a list or vector
#'
#' Character vectors are first converted to numeric values. Similarly, factors are first converted to 
#' their character representation and then to numeric values.
#' 
#' @param x vector of data
#' @return vector with non-finite (NaN, Inf) and missing data (NA) removed.
#' @examples
#' x <- 1:5
#' x[3]<-NA
#' x[4]<-1/0
#' x
#' ## [1]   1   2  NA Inf   5
#' finite.only(x)
#' ## [1] 1 2 5
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
finite.only <- function(x) {
  x<-safen(x);
  if(is.list(x)) {
    x[unlist(lapply(x,function(x) is.finite(x)))]
  } else if(is.vector(x)) {
    x[is.finite(x)];
  } else
    stop("unknown data structure");
}
