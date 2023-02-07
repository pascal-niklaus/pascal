#' Replace NA and NaN with zero
#'
#' Convenience function that replaces NA and NaN in vectors, lists, data frames, and matrices by zero
#'
#' Note that +Inf/-Inf values are preserved.
#' 
#' @param x vector of data
#' @return vector with non-finite (NaN, Inf) and missing data (NA) replaced by zero
#' @examples
#' x <- 1:5
#' x[3]<-NA
#' x[4]<-1/0
#' x
#' ## [1]   1   2  NA Inf   5
#' NAtozero(x)
#' ## [1] 1 2 0 0 5
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
NAtozero <- function(x) {
    if (is.data.frame(x)) {
        for (i in seq_along(names(x))) {
            if (is.numeric(x[, i])) {
                x[, i] <- ifelse(is.na(x[, i]) | is.nan(x[, i]), 0, x[, i])
            }
        }
        x
    } else if (is.list(x)) {
        lapply(x, NAtozero)
    } else if (is.matrix(x)) {
        apply(x, 1:2, NAtozero)
    } else {
        ifelse(is.na(x) | is.nan(x), 0, x)
    }
}
