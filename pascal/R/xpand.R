#' Pads a data.frame with additional rows
#'
#' \code{xpand} adds additional rows to a \code{data.frame} until the
#' groups specified have the same size.  This makes using
#' \code{asreml}'s outer product notation for residual correlations
#' easier. The added rows all contain \code{NA} values, except for
#' the key columns defining the groups.
#'
#' \code{asreml} requires a data frame with complete and equally sized
#' groups, should a residual covariance matrix be defined with the
#' outer product notation (options \code{rcov=~...} in ASReml3, and
#' \code{residuals=~...} in ASReml 4). While this notation is
#' convenient, padded 'unbalanced' data sets to the required format is
#' cumbersome.
#'
#' \code{xpand} does exactly this. \code{formala} is a right-hand side
#' only formula that contains the group-defining terms. When
#' \code{expand = TRUE}, all combinations of term values will be
#' included in the result, even if some groups are missing in the
#' original data frame.
#'
#' @param formula Right-sided formula indicating the columns that
#'     specify the groups.
#'
#' @param data \code{data.frame} with the data set to expand.
#'
#' @param cols string vector with names of columns to include in
#'     result, in addition to the ones specified in the formula
#'     (default = all).
#'
#' @param unit string with name of the column that holds the consecutive "units"
#'     in the groups (default = none)
#'
#' @param unname logical indicating whether row names referring to the
#'     original \code{data.frame} should be kept (default = TRUE).
#'
#' @param expand logical indicating whether all combination of terms
#'     should be included in the result, even if these do not occur in
#'     the original data (default = FALSE).
#'
#' @param verbose logical indicating whether some indications about
#'     data format and progress should be printed (default = FALSE)
#'
#' @return Data frame containing the expanded data
#'
#' @examples
#' data(CO2, package="datasets")
#' d <- CO2[c(1:5,31:32,48),]
#' ##    Plant        Type  Treatment conc uptake
#' ## 1    Qn1      Quebec nonchilled   95   16.0
#' ## 2    Qn1      Quebec nonchilled  175   30.4
#' ## 3    Qn1      Quebec nonchilled  250   34.8
#' ## 4    Qn1      Quebec nonchilled  350   37.2
#' ## 5    Qn1      Quebec nonchilled  500   35.3
#' ## 31   Qc2      Quebec    chilled  250   35.0
#' ## 32   Qc2      Quebec    chilled  350   38.8
#' ## 48   Mn1 Mississippi nonchilled  675   32.4
#'
#' xpand(~Type+Treatment, d)
#' ##             Type  Treatment Plant conc uptake
#' ## 1         Quebec nonchilled   Qn1   95   16.0
#' ## 2         Quebec nonchilled   Qn1  175   30.4
#' ## 3         Quebec nonchilled   Qn1  250   34.8
#' ## 4         Quebec nonchilled   Qn1  350   37.2
#' ## 5         Quebec nonchilled   Qn1  500   35.3
#' ## 31        Quebec    chilled   Qc2  250   35.0
#' ## 32        Quebec    chilled   Qc2  350   38.8
#' ## NA        Quebec    chilled  <NA>   NA     NA
#' ## NA.1      Quebec    chilled  <NA>   NA     NA
#' ## NA.2      Quebec    chilled  <NA>   NA     NA
#' ## 48   Mississippi nonchilled   Mn1  675   32.4
#' ## NA.3 Mississippi nonchilled  <NA>   NA     NA
#' ## NA.4 Mississippi nonchilled  <NA>   NA     NA
#' ## NA.5 Mississippi nonchilled  <NA>   NA     NA
#' ## NA.6 Mississippi nonchilled  <NA>   NA     NA
#'
#' xpand(~Type+Treatment, d, expand = TRUE)
#' ##              Type  Treatment Plant conc uptake
#' ## NA    Mississippi    chilled  <NA>   NA     NA
#' ## NA.1  Mississippi    chilled  <NA>   NA     NA
#' ## NA.2  Mississippi    chilled  <NA>   NA     NA
#' ## NA.3  Mississippi    chilled  <NA>   NA     NA
#' ## NA.4  Mississippi    chilled  <NA>   NA     NA
#' ## 31         Quebec    chilled   Qc2  250   35.0
#' ## 32         Quebec    chilled   Qc2  350   38.8
#' ## NA.5       Quebec    chilled  <NA>   NA     NA
#' ## NA.6       Quebec    chilled  <NA>   NA     NA
#' ## NA.7       Quebec    chilled  <NA>   NA     NA
#' ## 48    Mississippi nonchilled   Mn1  675   32.4
#' ## NA.8  Mississippi nonchilled  <NA>   NA     NA
#' ## NA.9  Mississippi nonchilled  <NA>   NA     NA
#' ## NA.10 Mississippi nonchilled  <NA>   NA     NA
#' ## NA.11 Mississippi nonchilled  <NA>   NA     NA
#' ## 1          Quebec nonchilled   Qn1   95   16.0
#' ## 2          Quebec nonchilled   Qn1  175   30.4
#' ## 3          Quebec nonchilled   Qn1  250   34.8
#' ## 4          Quebec nonchilled   Qn1  350   37.2
#' ## 5          Quebec nonchilled   Qn1  500   35.3
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @import datasets
#' @importFrom stats setNames
#' @export
xpand<- function (formula, data, verbose=FALSE, cols=NULL, unit=NULL, unname=FALSE, expand=FALSE) {
    sep <- "\U0001"
    mf <- match.call()
    m <- match(c("formula", "data"), names(mf))
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())

    ## determine size of largest group
    g.orig <- unname(apply(mf, 1,function(x) paste(as.character(x), collapse=sep)))
    sz <- table(g.orig)
    n <- max(sz)

    if (verbose) {
        cat(length(sz),"groups\n")
        cat("smallest/largest group: ",min(sz),"/",n,"\n",sep="")
    }

    ## keys will hold complete set of group keys
    if (!expand) {
        keys  <- mf[!duplicated(mf),]
        rownames(keys) <- NULL
    } else {
        terms  <- lapply(
            setNames(names(mf), names(mf)),
            function(x) suc(get(x, envir = as.environment(mf))))
        keys  <- do.call("expand.grid", terms)
    }

    keys <- keys[rep(1:nrow(keys), each=n),]
    rownames(keys) <- NULL

    gvec <- unique(unname(apply(keys,1,function(x) paste(as.character(x),collapse=sep))))

    if (!is.null(unit))
        keys[[unit]]  <- factor(sprintf("unit-%0*d",1+floor(log(n,10)),1:n))

    cidx <- if (!is.null(cols)) match(cols, names(data)) else 1:ncol(data)
    cidx <- setdiff(cidx, match(names(keys), names(data)))

    idx <- as.numeric(
        sapply(gvec,
               function(g_) {
                   c(which(g.orig == g_), rep(NA, n))[1:n]
               }))
    r <- data.frame(keys, data[idx, cidx, drop=FALSE])
    if (unname)
        rownames(r) <- NULL
    r
}
