#' Rename columns in data.frame
#'
#' Rename columns in data frame.
#'
#' @param d \code{data.frame}
#'
#' @param cols names of columns, in the form \code{"new=old"}.
#'
#' @return data frame with renamed columns
#' @examples
#'
#' data(CO2, package="datasets")
#'
#' d <- renameColumns(CO2,c("type=Type","CO2=conc"))
#' head(d)
#' ##   Plant   type  Treatment CO2 uptake
#' ## 1   Qn1 Quebec nonchilled  95   16.0
#' ## 2   Qn1 Quebec nonchilled 175   30.4
#' ## 3   Qn1 Quebec nonchilled 250   34.8
#' ## 4   Qn1 Quebec nonchilled 350   37.2
#' ## 5   Qn1 Quebec nonchilled 500   35.3
#' ## 6   Qn1 Quebec nonchilled 675   39.2
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
renameColumns <- function(d, cols) {
    tcols <- gsub("^([^=]+) *= *(.+)$", "\\1", cols, perl = TRUE)
    scols <- gsub("^([^=]+) *= *(.+)$", "\\2", cols, perl = TRUE)
    names(d)[match(scols, names(d))] <- tcols
    d
}
