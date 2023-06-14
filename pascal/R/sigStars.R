#' Convert P values to stars indicating significance levels
#'
#' This function essentially converts a vector of P values to "stars"
#' indicating significance levels.  It returns \code{***} for
#' x<=0.001, \code{**} for P<=0.01, \code{*} for P<=0.05, \code{(*)}
#' for P<=0.1, and \code{n.s.} for all other values.
#'
#' @param x Vector with P values. If a character vector is passed, it
#'     first is converted to numeric values by calling
#'     \code{safen(...)}. This even works for factors since these are
#'     first converted to plain character vectors.
#'
#' @return Vector of corresponding strings indicating significance
#'     levels.
#'
#' @examples
#' sigStars(c(0.0001,0.3,.09,.7))
#' ## [1] "***"  "n.s." "(*)"  "n.s."
#' sigStars(factor(c("0.0001","0.04"))) # not very sensible call...
#' ## [1] "***" "*" se(1:5)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#'
#' @keywords utilities, misc
#'
#' @export
sigStars <- function(x) {
    sapply(safen(x), function(p) {
        if (!is.finite(p)) "-" else if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else if (p <= 0.1) "(*)" else "n.s."
    })
}
