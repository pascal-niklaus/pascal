#' Delete columns in data.frame
#'
#' Delete columns in data frame.
#'
#' @param d \code{data.frame}
#'
#' @param cols names of columns to move around in the data frame. The
#'     columns can be renamed by using the form \code{"new=old"}.
#'
#' @param rx logical indicating whether \code{cols} is a regular expression
#'
#' @return data frame with columns removed
#' @examples
#'
#' data(CO2, package="datasets")
#'
#' CO2[1,]
#' ##   Plant   Type  Treatment conc uptake
#' ## 1   Qn1 Quebec nonchilled   95     16
#'
#' d <- deleteColumns(CO2, "conc")
#' d[1,]
#' ##   Plant CO2 rate   Type  Treatment
#' ## 1   Qn1  95   16 Quebec nonchilled
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
deleteColumns <- function(d, cols, rx = FALSE) {
    if (rx) {
        idx <- which(grepl(cols, names(d), perl = TRUE))
    } else {
        idx <- match(cols, names(d))
    }
    if (any(is.na(idx)) || any(idx < 1))
        stop("Columns not present in data frame")

    d[, -idx]
}
