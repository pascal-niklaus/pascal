#' Extract Captured Subexpressions from Regular Expressions
#'
#' Return captured subexpressions as data frame.
#'
#' This function is copied with slight modifications from
#' https://gist.github.com/MrFlick/10413321. It basically returns
#' subexpressions captured by \code{regexpr} and its relatives.  The
#' captured subexpressions are returned as a data frame.
#'
#' @param x Texts that were matched
#' @param m Matches as returned by (g)regexpr
#' @return Data frame containing the matched substrings, or NA if none
#'     matched. The columns have the corresponding names if named
#'     subexpressions (\code{(?<name>...)}) were used.
#'
#' @seealso \code{\link{regexpr}}
#' @examples
#' txt <- c("word X123,Y456 xyz","X23 , Y17", "none")
#' x <- regexpr("^.*X(?<x>\\d+) *, *Y(?<y>\\d+).*$", txt, perl=TRUE)
#' rxmatches(txt,x)
#' ##      x     y
#' ## [1,] "123" "456"
#' ## [2,] "23"  "17"
#' ## [3,] NA    NA
#' @author MrFlick \url{https://github.com/MrFlick}, copied by Pascal
#'     Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
rxmatches <- function(x,m) {
    if (length(x) != length(m))
        stop(gettextf("%s and %s must have the same length",
                      sQuote("x"), sQuote("m")), domain = NA)
    ili <- is.list(m)
    useBytes <- if (ili) {
                    any(unlist(lapply(m, attr, "useBytes")))
                } else {
                    any(attr(m, "useBytes"))
                }
    if (useBytes) {
        asc <- iconv(x, "latin1", "ASCII")
        ind <- is.na(asc) | (asc != x)
        if (any(ind))
            Encoding(x[ind]) <- "bytes"
    }
    if (ili) {
        if (any(sapply(m, function(x) {is.null(attr(x,"capture.start"))})==T)) {
            stop("No capture data found (did you use perl=T?)")
        }
        starts<-lapply(m, function(x) {attr(x, "capture.start")})
        lengths<-lapply(m, function(x) {attr(x, "capture.length")})
    } else {
        if (is.null(attr(m,"capture.start"))) {
            stop("No capture data found (did you use perl=T?)")
        }
        x<-list(x)
        starts<-list(attr(m, "capture.start"))
        lengths<-list(attr(m, "capture.length"))
    }

    cleannames <- function(x) {
        if (!is.null(colnames(x))) {
            colnames(x) <- make.unique(make.names(colnames(x)))
        }
        x
    }
    starts <- lapply(starts, cleannames)
    lengths <- lapply(lengths, cleannames)

    Substring<-function(x,starts,lens) {
        if(all(starts<0)) {
            return()
        } else {
            x <- t(
                mapply(function(x,st,ln) ifelse(st<0,NA,substring(x,st,st+ln-1)),
                       x, data.frame(t(starts)), data.frame(t(lens)),
                       USE.NAMES=FALSE)
            )
            if (!is.null(colnames(starts))) {
                colnames(x)<-colnames(starts)
            }
            x
        }
    }

    y<-Map(
        function(x, sos, mls) {
            Substring(x,sos,mls)
        },
        x,
        starts,
        lengths,
        USE.NAMES = FALSE
    )

    if (ili) {
        as.data.frame(y, stringsAsFactors = FALSE)
    } else {
        as.data.frame(y[[1]], stringsAsFactors = FALSE)
    }
}
