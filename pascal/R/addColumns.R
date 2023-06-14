#' Add columns to data.frame
#'
#' Adds columns from another data.frame based on a key.
#'
#' @param trg,src target and source \code{data.frame}s
#'
#' @param key key column(s). Either a single name, in which case it
#'     applies to both source and target, or a string of the form
#'     \code{"key1=key2"} where the first and second apply to target and
#'     source, respectively. Multiple keys may be passed.
#'
#' @param cols names of columns to copy to target. The columns can be
#'     renamed by using the form \code{"new=old"}.
#'
#' @param after,before string. Indicates after or before which of the
#'     existing columns the new columns should be inserted. Default is
#'     to append them to the right of existing columns.
#'
#' @return target data frame amended with columns.
#' @examples
#' data(CO2, package="datasets")
#'
#' d <- aggr(CO2,"Plant", "mean.uptake=mean(uptake)")
#' addColumns(d, CO2, "Plant", c("Type","Treatment"))
#'
#' ## differnet key columns, and column renaming
#' d <- aggr(CO2,"p=Plant", "mean.uptake=mean(uptake)")
#' addColumns(d, CO2, p="Plant", c("name=Type","chilled=Treatment"))
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
addColumns <- function(trg, src, key, cols, after=NULL, before=NULL) {
    key <- lapply(strsplit(key, "="), function(x) rep(x, 2)[1:2])
    trgidx <- sapply(key, function(x) which(names(trg)==x[1]))
    srcidx <- sapply(key, function(x) which(names(src)==x[2]))
    trgkey <- apply(trg[, trgidx, drop=FALSE], 1, function(x) paste(x, collapse="\u2055"))
    srckey <- apply(src[, srcidx, drop=FALSE], 1, function(x) paste(x, collapse="\u2055"))
    idx <- match(trgkey, srckey)
    if (max(sapply(trgkey, function(x) sum(srckey==x)))>1)
        warning("Match of keys is not unique: returning values of first match")

    if (is.null(after) && is.null(before))
        icol <- ncol(trg)
    else
        icol <- which(names(trg) %in% c(after, before))
    if (length(icol) != 1)
        stop("Illegal 'after' or 'before' column specified")

    tcols <- gsub("^([^=]+) *= *(.+)$", "\\1", cols, perl=TRUE)
    scols <- gsub("^([^=]+) *= *(.+)$", "\\2", cols, perl=TRUE)

    for(v in seq_along(scols))
        trg[[tcols[v]]] <- src[[scols[v]]][idx]

    tcol <- match(tcols, names(trg))
    if (is.null(before))
        trg[,c(setdiff(1:icol, tcol),
               tcol,
               setdiff(1:ncol(trg), c(1:icol,tcol)))]
    else
        trg[,c(setdiff(1:icol, c(icol, tcol)),
               unique(c(tcol, icol)),
               setdiff(1:ncol(trg), c(1:icol, tcol)))]
}
