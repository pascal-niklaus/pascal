#' Convert factor/character data to numeric values
#'
#' \code{safen} will return the numeric representation of data stored
#' in a vector, list, or matrix.  Numeric data are preserved,
#' character data are converted to numeric.  Factors are first
#' converted to character and then to numeric, thus avoiding returning
#' data that reflects factor levels \code{1,2,3,...} rather than the
#' numeric value represented by the character string coding for the
#' level.
#'
#' @param x A vector, list or matrix containing character or numeric
#'     data, possibly as factor.
#'
#' @return A numeric vector, list or matrix containing the converted
#'     data.
#'
#' @examples
#' library(pascal)
#' x <- factor(3:5)
#' x
#' ## [1] 3 4 5
#' ## Levels: 3 4 5
#' as.numeric(x)
#' ## [1] 1 2 3
#' safen(x)
#' ## [1] 3 4 5
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
safen <- function(x) {
    if (is.list(x)) {
        lapply(x, safen)
    } else if (is.matrix(x)) {
        apply(x, 1:2, safen)
    } else {
        if (is.factor(x)) {
            as.numeric(as.character(x))
        } else if (is.character(x)) {
            as.numeric(x)
        } else {
            x
        }
    }
}
