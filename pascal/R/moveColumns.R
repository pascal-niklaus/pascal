#' Move columns in data.frame
#'
#' Move columns in data frame to another position, possibly renaming
#' them.
#'
#' @param d \code{data.frame}
#'
#' @param cols names of columns to move around in the data frame. The
#'     columns can be renamed by using the form \code{"new=old"}.
#'
#' @param after,before string. Indicates after or before which of the
#'     existing columns the columns should be placed.
#'
#' @return data frame with different column order
#' @examples
#'
#' data(CO2, package="datasets")
#'
#' CO2[1,]
#' ##   Plant   Type  Treatment conc uptake
#' ## 1   Qn1 Quebec nonchilled   95     16
#'
#' d <- moveColumns(CO2, c("CO2=conc", "rate=uptake"), after="Plant")
#' d[1,]
#' ##   Plant CO2 rate   Type  Treatment
#' ## 1   Qn1  95   16 Quebec nonchilled
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
moveColumns <- function(d, cols, after=NULL, before=NULL) {
    tcols <- gsub("^([^=]+) *= *(.+)$", "\\1", cols, perl = TRUE)
    scols <- gsub("^([^=]+) *= *(.+)$", "\\2", cols, perl = TRUE)
    idx <- match(scols, names(d))
    if (any(is.na(idx)) || any(idx < 1) || any(idx > ncol(d)))
        stop("Columns not present in data frame")

    if (is.null(after) && is.null(before))
        icol <- ncol(d)
    else if (is.null(before))
        icol <- which(names(d) == after)
    else
        icol <- which(names(d) == before) - 1

    names(d)[idx] <- tcols

    cidx.mv <- idx
    cidx <- setdiff(seq_along(names(d)), cidx.mv)
    cidx.left <- Filter(function(x) x <= icol, cidx)
    cidx.right <- setdiff(cidx, cidx.left)

    d[, c(cidx.left, cidx.mv, cidx.right)]
}
