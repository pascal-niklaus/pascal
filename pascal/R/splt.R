#' Split a data frame
#'
#' Given a data frame, one or several columns are split into several new ones;
#' the rows that define how the columns are split are defined by the grouping
#' variables \code{by}.
#'
#' \code{splt} and the corresponding \code{stck} were written as a
#' generic way to break up and combine individual data columns.
#'
#' The \code{factors} (these can also be continuous covariables) that
#' are to be preserved in the split data frame are passed as character
#' string and can be given new names in the stacked data set. For
#' example, \code{factors=c("f1=factor1","f2=factor2")} will rename
#' the factors \code{factor1} and \code{factor2} to \code{f1} and
#' \code{f2}.  If no new name is given, the original name is kept.
#'
#' The columns that are to be split are passed in \code{to.split} in the
#' form \code{to.split=c("newcol1=col1","newcol2=col2")}.
#'
#' The newly generated columns will have names \code{newcol1.bylev1},
#' \code{newcol2.bylev2} etc, where bylev1, bylev2 are the levels of
#' the \code{by} variable.  For example, to.split=c("m=biomass"),
#' by=c("year") will generate the new columns \code{m.1992},
#' \code{m.1993}, \code{m.1994} if \code{year} consists of the levels
#' \code{1992}, \code{1993} and \code{1994}.  The 'by' part of the
#' columns to be created can also be specified explicitely in
#' new.names in the form
#' \code{new.names=c("1992","1993","1994")}. Note that the order
#' refers to ascending alphabetical order of the levels of the 'by'
#' variables, not to the order in which these occur in the data frame!
#'
#' if \code{expand=TRUE}, all combinations of factors passed will
#' exist in the newly created data frame and the \code{to.split}
#' columns padded with NAs accordingly.
#'
#' @param d Source data frame containing the data set to split
#'
#' @param by Character vector containing the names of the factors that
#'     define the groups according to which columns are split
#'
#' @param to.split Character vector containing the names of the
#'     columns to be split
#'
#' @param factors Character vector containing the names of the factors
#'     that are preserved in the new data set; factors can be renamed
#'     by passing them in the form \code{new_name=old_name}.
#'
#' @param new.names Character vector containing the parts of the
#'     identifiers that identify the 'by'-groups in the newly created
#'     columns.  Their order refers to the the 'by' variable sorted in
#'     ascending alphabetical order; when several 'by' variables are
#'     passed, the order refers to the ascending alphabetical order of
#'     the by variables pasted together using a ':'.
#'
#' @param expand Logical flag: if TRUE, the resulting data frame will
#'     contain all combinations of the supplied factors, even if these
#'     are not present in the original data frame
#'
#' @param sep Separator. Used internally and should not occur in
#'     factor levels
#'
#' @return Data frame containing the split data
#'
#' @seealso \code{\link{aggregate}}, \code{\link{expand.grid}}
#' @examples
#' data(CO2, package = "datasets")
#' d <- CO2
#' d$Replicate<-substr(as.character(d$Plant), 3, 3)
#' d.split <- splt(d,
#'                 factors = c("Replicate", "Type", "conc"),
#'                 by = "Treatment", to.split=c("uptake"))
#' d.split <- d.split[with(d.split,
#'                         order(Replicate, Type, conc)),]
#' d[d$Type=="Mississippi" & d$Replicate==1,]
#' ##    Plant        Type  Treatment conc uptake Replicate
#' ## 43   Mn1 Mississippi nonchilled   95   10.6         1
#' ## 44   Mn1 Mississippi nonchilled  175   19.2         1
#' ## 45   Mn1 Mississippi nonchilled  250   26.2         1
#' ## 46   Mn1 Mississippi nonchilled  350   30.0         1
#' ## 47   Mn1 Mississippi nonchilled  500   30.9         1
#' ## 48   Mn1 Mississippi nonchilled  675   32.4         1
#' ## 49   Mn1 Mississippi nonchilled 1000   35.5         1
#' ## 64   Mc1 Mississippi    chilled   95   10.5         1
#' ## 65   Mc1 Mississippi    chilled  175   14.9         1
#' ## 66   Mc1 Mississippi    chilled  250   18.1         1
#' ## 67   Mc1 Mississippi    chilled  350   18.9         1
#' ## 68   Mc1 Mississippi    chilled  500   19.5         1
#' ## 69   Mc1 Mississippi    chilled  675   22.2         1
#' ## 70   Mc1 Mississippi    chilled 1000   21.9         1
#' d.split[d.split$Type == "Mississippi" & d.split$Replicate == 1, ]
#' ##   Replicate        Type conc uptake.chilled uptake.nonchilled
#' ## 7         1 Mississippi   95           10.5              10.6
#' ## 2         1 Mississippi  175           14.9              19.2
#' ## 3         1 Mississippi  250           18.1              26.2
#' ## 4         1 Mississippi  350           18.9              30.0
#' ## 5         1 Mississippi  500           19.5              30.9
#' ## 6         1 Mississippi  675           22.2              32.4
#' ## 1         1 Mississippi 1000           21.9              35.5
#'
#' d.stacked <- stck(d.split,
#'                   factors=c("Replicate", "Type", "conc"),
#'                   to.stack=c("uptake=uptake.chilled,uptake.nonchilled"),
#'                   cat.names=c("Treatment=chilled,nonchilled"))
#' d.stacked$conc <- safen(d.stacked$conc)
#' d.stacked <- d.stacked[with(d.stacked,
#'                             order(Replicate, Type, Treatment,conc)), ]
#' d.stacked[d.stacked$Type=="Mississippi" & d.stacked$Replicate==1,]
#' ##    Replicate        Type conc  Treatment uptake
#' ## 1          1 Mississippi 1000    chilled   21.9
#' ## 2          1 Mississippi  175    chilled   14.9
#' ## 3          1 Mississippi  250    chilled   18.1
#' ## 4          1 Mississippi  350    chilled   18.9
#' ## 5          1 Mississippi  500    chilled   19.5
#' ## 6          1 Mississippi  675    chilled   22.2
#' ## 7          1 Mississippi   95    chilled   10.5
#' ## 43         1 Mississippi 1000 nonchilled   35.5
#' ## 44         1 Mississippi  175 nonchilled   19.2
#' ## 45         1 Mississippi  250 nonchilled   26.2
#' ## 46         1 Mississippi  350 nonchilled   30.0
#' ## 47         1 Mississippi  500 nonchilled   30.9
#' ## 48         1 Mississippi  675 nonchilled   32.4
#' ## 49         1 Mississippi   95 nonchilled   10.6
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#'
#' @keywords manip datagen utilities misc
#'
#' @export
splt <- function(d, by, to.split, factors = NULL,
                 new.names = NULL, expand = FALSE, sep = ":") {
    ## process factors
    if (!is.null(factors)) {
        facnames <- sapply(
            factors,
            function(x) rev(unlist(strsplit(x, "="), use.names = FALSE))[1]
        )

        facnewnames <- sapply(
            factors,
            function(x) unlist(strsplit(x, "="), use.names = FALSE)[1]
        )

        faccols <- match(facnames, names(d))

        factypes <- sapply(
            seq_along(faccols),
            function(i) {
                if (is.factor(d[, faccols[i]]))
                    "as.factor"
            else
                paste0("as.", typeof(d[, faccols[i]]))
            }
        )

        if (any(is.na(faccols)))
            stop("splt: Non-existing column specified in 'factors'")

        if (length(faccols))
            levs <- .colpaste(d, faccols, sep)
    } else {
        faccols <- integer(0)
        facnames <- integer(0)
        facnewnames <- integer(0)
        levs <- integer(0)
    }

    ## process by columns
    bynames <- by
    bycols <- match(bynames, names(d))

    if (any(is.na(bycols)))
        stop("splt: Non-existing column specified in 'by'")

    bylevs <- .colpaste(d, bycols, sep)
    alllevs <- .colpaste(d, c(bycols, faccols), sep)

    ## add extra 'factor' if rows not uniquely specified
    rownadded <- FALSE
    if (any(duplicated(alllevs))) {
        warning("splt: 'by' and 'factors' do not specify rows unequivocally")
        rownadded <- TRUE
        d$..n..splt..[order(alllevs)] <-
            unlist(
                lapply(
                    unique(alllevs),
                    function(x) seq_along(which(alllevs == x))
                ),
                use.names = FALSE
            )

        facnames <- c("..n..splt..", facnames)
        facnewnames <- c("..n..splt..", facnewnames)
        faccols <- match(facnames, names(d))

        alllevs <- .colpaste(d, c(bycols, faccols), sep)
        levs <- .colpaste(d, faccols, sep)
    }

    ## create factor skeleton
    if (expand) {
        faclist <- lapply(
            faccols,
            function(x) sort(unique(d[, x]))
        )
        attr(faclist, "names") <- facnewnames
        dnew <- expand.grid(faclist)
    } else {
        lev <- sort(unique(levs))   # unique
        if (length(faccols) == 1) { # single factor case (vector/matrix)
            dnew <- data.frame(lev)
        } else {
            dnew <- data.frame(t(sapply(lev,
                function(x) {
                    unlist(strsplit(x, sep),
                        use.names = FALSE
                    )
                },
                USE.NAMES = FALSE
            )))
        }
        colnames(dnew) <- facnewnames
    }

    rownames(dnew) <- .colpaste(dnew, sep = sep)

    ## restore column types
    for (i in seq_along(faccols))
        dnew[, i] <- do.call(factypes[i], list(dnew[, i]))

    levs_bylevs <- paste(levs, bylevs, sep = sep)

    ## loop over no of vars to split
    for (v in to.split) {
        v.old <- rev(unlist(strsplit(v, "=")))[1]
        v.new <- (unlist(strsplit(v, "="), use.names = FALSE))[1]
        b.vec <- sort(unique(bylevs))
        b.newnames <- if (is.null(new.names)) b.vec else new.names
        b.newnames <- gsub("[^A-Za-z0-9_]", ".", b.newnames)

        if (length(b.newnames) != length(b.vec))
            stop("splt: number of names passed in 'new.names' ",
                 "is unequal to the number of groups defined by 'by'.")

        ## loop over blocks to split
        for (b.index in seq_along(b.vec)) {
            idx <- match(
                paste(rownames(dnew), b.vec[b.index], sep = sep),
                levs_bylevs
            )
            v.new2 <- paste(
                v.new,
                gsub(sep, "_", b.newnames[b.index], fixed = TRUE),
                sep = "."
            )
            dnew[[v.new2]] <- d[[v.old]][idx]
        }
    }

    row.names(dnew) <- NULL
    if (rownadded)
       dnew <- dnew[, -which(names(dnew) == "..n..splt..")]
    dnew
}

.colpaste <- function(d, bycols = NULL, sep = ":") {
    if (is.null(bycols))
        tmp <- as.list(d)
    else
        tmp <- as.list(d[, bycols, drop = FALSE])
    tmp$sep <- sep
    do.call(paste, tmp)
}
