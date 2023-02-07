#' Aggregates a Data Frame
#'
#' \code{aggr} was written in 2005 as a more generic alternative to
#' \code{\link{aggregate}}.Meanwhile, other libraries exist that
#' provide functions to achieve similar goals, including \code{plyr}.
#'
#' The \code{factors} defining the grouping are given as character
#' string and can be given new names in the aggregated data set. For
#' example, \code{factors=c("f1=factor1","f2=factor2")} will rename
#' the factors \code{factor1} and \code{factor2} to \code{f1} and
#' \code{f2}. If no new name is given, the original is kept.
#'
#' The summary column defined in \code{newcols} is passed as character
#' vector with the form
#' \code{new_name=summary_function(old_name)}. Any defined function
#' taking a numeric vector as argument can be used.  For example,
#' \code{newcols=c("m.tot=sum(m)","m.avg=mean(m)","n=length(m)")} will
#' compute the sum, average and number of data in \code{m} for all
#' factor combinations and return them under the names \code{m.tot},
#' \code{m.avg} and \code{n}.  The R code given is not fully parsed;
#' instead, any name enclosed in parenthesis (without whitespace) is
#' treated as column name (textual replacement).  This allows for
#' terms such as \code{newcols=c("m.tot=sum((m),na.rm=T)")}.  As a
#' trade-off, \code{newcols=c("m.tot=sum(( m ),na.rm=T)")} will not
#' work because there is whitespace around the variable name
#' \code{m}. This can be used to distinguish between column names and
#' other variables defined in the scope in which \code{aggr} is
#' executed.
#'
#' If the package \code{parallel} is installed, some computations are
#' parallelized on platforms other than Windows, using all available
#' processor cores.  However, the performance gain is small
#' for simple summary functions.
#'
#' @param d Source data frame containing the data set to aggregate
#'
#' @param factors Character vector containing the names of the columns
#'     that define the categories in the aggregated data set
#'
#' @param newcols Character vector containing the names of the
#'     aggregated variables and the function with which they are
#'     calculated
#'
#' @param expand Logical flag: if TRUE, the resulting data frame will
#'     contain all combinations of the supplied factors, even if these
#'     are not present in the original data frame
#'
#' @param parallel Logical indicating whether calculation of
#'     aggregated values should be parallelized, using \code{mclapply}
#'     (library \code{parallel}) is available. Note that
#'     parallelization incurs an overhead and only results in a
#'     performance benefit for complex summary functions.
#'
#' @return Data frame containing the aggregated data
#'
#' @seealso \code{\link{aggregate}}
#' @examples
#' data(CO2, package="datasets")
#' CO2[1:3,]
#' ##   Plant   Type  Treatment conc uptake
#' ## 1   Qn1 Quebec nonchilled   95   16.0
#' ## 2   Qn1 Quebec nonchilled  175   30.4
#' ## 3   Qn1 Quebec nonchilled  250   34.8
#' aggr(CO2,c("Treatment","CO2.conc=conc"),c("uptake=mean(uptake)","se.uptake=se(uptake)","n=length(uptake)"))
#' ##     Treatment CO2.conc   uptake se.uptake n
#' ## 1     chilled     1000 29.78333 5.0665844 6
#' ## 2     chilled      175 19.45000 2.4031576 6
#' ## 3     chilled      250 25.28333 4.3148130 6
#' ## 4     chilled      350 26.20000 4.4218397 6
#' ## 5     chilled      500 26.65000 4.6725974 6
#' ## 6     chilled      675 27.88333 4.4737320 6
#' ## 7     chilled       95 11.23333 1.1678090 6
#' ## 8  nonchilled     1000 37.38333 2.8814830 6
#' ## 9  nonchilled      175 25.11667 2.3318686 6
#' ## 10 nonchilled      250 32.46667 2.4184936 6
#' ## 11 nonchilled      350 35.13333 2.4968870 6
#' ## 12 nonchilled      500 35.10000 2.3066570 6
#' ## 13 nonchilled      675 36.01667 2.5896482 6
#' ## 14 nonchilled       95 13.28333 0.9792571 6
#' CO2.red <- CO2[CO2$conc!=250 | CO2$Treatment!="chilled",]
#' aggr(CO2.red,c("Treatment","CO2.conc=conc"),c("uptake=mean(uptake)","n=length(uptake)"),expand=TRUE)
#' ##     Treatment CO2.conc   uptake n
#' ## 1     chilled     1000 29.78333 6
#' ## 2  nonchilled     1000 37.38333 6
#' ## 3     chilled      175 19.45000 6
#' ## 4  nonchilled      175 25.11667 6
#' ## 5     chilled      250      NaN 0
#' ## 6  nonchilled      250 32.46667 6
#' ## 7     chilled      350 26.20000 6
#' ## 8  nonchilled      350 35.13333 6
#' ## 9     chilled      500 26.65000 6
#' ## 10 nonchilled      500 35.10000 6
#' ## 11    chilled      675 27.88333 6
#' ## 12 nonchilled      675 36.01667 6
#' ## 13    chilled       95 11.23333 6
#' ## 14 nonchilled       95 13.28333 6
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @import datasets
#' @export
aggr <- function(d,
                 factors = NULL,
                 newcols = NULL,
                 expand = FALSE,
                 parallel = FALSE) {
    parallel <- ((Sys.info()["sysname"] != "Windows") &&
        requireNamespace("parallel")) &&
        parallel
    n.cores <- if (parallel) parallel::detectCores() else 1

    ## factors
    facnames <- sapply(
        factors,
        function(x) rev(unlist(strsplit(x, "="), use.names = FALSE))[1]
    )
    facnewnames <- sapply(
        factors,
        function(x) unlist(strsplit(x, "="), use.names = FALSE)[1]
    )

    if (!all(facnames %in% names(d))) {
        stop(
            "Grouping factor(s) ",
            paste("'", facnames[!facnames %in% names(d)], "'",
                sep = "", collapse = ", "
            ),
            " is/are not part of the data frame"
        )
    }

    faccols <- match(facnames, names(d))

    factypes <- sapply(
        seq_along(faccols),
        function(i) {
            if (is.factor(d[, faccols[i]])) {
                "as.factor"
            } else {
                paste0("as.", typeof(d[, faccols[i]]))
            }
        }
    )

    ## make sure categories are factors, since numeric level codes used later
    for (i in faccols) {
        if (!is.factor(d[, i])) {
            d[, i] <- as.factor(d[, i])
        }
    }

    ## until 9999 levels, the factor order is preserved in the results
    ## above that, the results are still correct but not ordered...
    levs <- .colpaste(
        as.data.frame(lapply(d[, faccols, drop = FALSE],
                             function(i) sprintf("%04d", i)))
    )

    ## create factor skeleton
    if (expand) {
        faclist <- lapply(
            faccols,
            function(i) sort(unique(sprintf("%04d", as.integer(d[, i]))))
        )
        attr(faclist, "names") <- facnewnames
        dnew <- expand.grid(faclist)
    } else {
        lev <- sort(unique(as.character(levs)))
        dnew <- data.frame(t(as.data.frame(strsplit(lev, ":"))))
        colnames(dnew) <- facnewnames
    }
    rownames(dnew) <- .colpaste(dnew, sep = ":")

    ## replace codes by factor levels
    for (i in seq_along(faccols))
        dnew[, i] <-
            levels(d[[faccols[i]]])[as.integer(as.character(dnew[, i]))]

    ## restore column type
    for (i in seq_along(factypes))
        dnew[, i] <- do.call(factypes[i], list(dnew[, i]))

    eqpos <- sapply(newcols,
                    function(x) attr(regexpr("[^=]+=", x), "match.length"))

    newnames <- substr(newcols, 1, eqpos - 1)
    funcpart <- substr(newcols, eqpos + 1, nchar(newcols))

    ## check that all variables are in data frame...
    ## leads to less cryptic error messages
    ynames <- unlist(
        lapply(funcpart,
               function(f) {
                   m <- gregexpr("(?<=\\()[^\\(\\), ]+(?=\\))", f, perl = TRUE)[[1]]
                   sapply(which(m > 0),
                          function(n) {
                              substr(f, m[n], m[n] - 1 + attr(m, "match.length")[n])
                          })
               }))

    if (! all(ynames %in% names(d)))
       stop("Variable ",
            paste("'", ynames[! ynames %in% names(d)], "'",
                  sep = "", collapse = ", "),
            " is/are not part of the data frame")

    ## modify expressions to contain grouping
    funcpart <- gsub("\\(([^,\\(\\) ]+)\\)",
                     "(d$\\1[idx])",
                     funcpart,
                     perl = TRUE)

    ## calculate aggregated column data
    ## uses parallelization if package 'parallel' is installed
    for (i in seq_along(newcols)) {
        cmd <- if (parallel)
                   paste0("dnew$", newnames[i], " <- ",
                          "parallel::mcmapply(function(x) { idx<-levs==x;",
                          funcpart[i],
                          " },rownames(dnew),USE.NAMES=FALSE,mc.cores=",
                          n.cores,
                          ")")
               else
                   paste0("dnew$", newnames[i], "<-",
                          "sapply(rownames(dnew),function(x) { idx<-levs==x;",
                          funcpart[i],
                          " } )")
        eval(str2lang(cmd))
    }

    rownames(dnew) <- NULL
    dnew
}
