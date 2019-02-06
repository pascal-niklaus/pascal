#' Add columns to data.frame
#'
#' Adds columns from another data.frame based on a key.
#'
#' This is useful when In meta-analyses, one often uses Z-transformed effect sizes.  This
#' function returns such effect sizes, computed from F values and
#' associated nominator and denominator degrees of freedom.
#'
#' @param trg,src target and source \code{data.frame}s
#'
#' @param key key column(s). Either a single name, in which case it
#'     applies to both source and target, or a vector of two, with
#'     first and second applying to target and source, respectively.
#'
#' @param cols names of columns to copy to target. The columns can be
#'     renamed by using the form \code{"new=old"}.
#'
#' @param after logical. Indicated whether the new columns are placed
#'     right after the key column (default, after=TRUE), or before the
#'     key column (adter=FALSE). The new columns always are in the
#'     order in which they were specified.
#'
#' @return target data frame amended with columns.
#' @examples
#'
#' data(CO2, package="datasets")
#'
#' d <- aggr(CO2,"Plant", "mean.uptake=mean(uptake)")
#' addColumns(d, CO2, "Plant",c("Type","Treatment"))
#'
#' ## differnet key columns, and column renaming
#' d <- aggr(CO2,"p=Plant", "mean.uptake=mean(uptake)")
#' addColumns(d, CO2, c("p","Plant"),c("name=Type","chilled=Treatment"))
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
addColumns <- function(trg, src, key, cols, after=TRUE) {
    key <- rep(key,2)[1:2]
    icol <- which(names(trg)==key[1])
    idx <- match(trg[[key[1]]],src[[key[2]]])
    tcols <- gsub("^([^=]+) *= *(.+)$","\\1",cols,perl=TRUE)
    scols <- gsub("^([^=]+) *= *(.+)$","\\2",cols,perl=TRUE)
    for(v in seq_along(scols))
        trg[[tcols[v]]] <- src[[scols[v]]][idx]
    tcol <- match(tcols, names(trg))
    if(after)
        trg[,c(1:icol, tcol, setdiff(1:ncol(trg),c(1:icol,tcol)))]
    else
        trg[,c(setdiff(1:icol,icol), tcol, icol, setdiff(1:ncol(trg),c(1:icol,tcol)))]
}
