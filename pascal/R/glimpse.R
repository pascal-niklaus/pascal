#' Print an excerpt of some data
#'
#' This function allows to print an excerpt of some data structures
#' (currently data.frame, vector, list).  The first and last 'len'
#' elements are printed, with '...' in between the head and tail
#' parts.
#'
#' I use this function in status reports or log files to show what
#' type of output is produced.
#'
#' @param ... data to be printed
#'
#' @param len number of elements to be printed of head and tail
#'     section of the data.
#'
#' @param pre,post,between number of empty lines to print before and after
#'     the output, and in between elements if there are multiple.
#'
#' @examples
#' data(CO2)
#' a <- 1:20
#' b <- as.list(LETTERS[1:20])
#' glimpse(CO2,a,b,len=3)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
glimpse <- function(..., len=5,pre=1,post=0,between=1) {
    n <- setdiff(as.character(match.call(expand.dots=TRUE)),
                   as.character(match.call(expand.dots=FALSE)))
    x <- list(...)
    for(j in rep(0,pre))
            cat("\n")
    for(i in seq_along(x)) {
        if(i > 1)
            for(j in rep(0, between))
                cat("\n")
        obj <- x[[i]]
        if("data.frame" %in% class(obj)) {
            cat("--- data.frame:",n[i],"\n\n")
            show.all <- nrow(obj) <= 2*len
            if(show.all)
                print(obj)
            else {
                idx <- c(1:len,(nrow(obj)-len+1):nrow(obj))
                txt <- capture.output(print(obj[idx,]),file=NULL)
                tl <- length(txt)-(len-1):0
                hd <- 1:(length(txt)-len)
                for(i in hd)
                    cat(txt[i],"\n")
                cat("...\n")
                for(i in tl)
                    cat(txt[i],"\n")
            }
        } else if(is.list(obj)) {
            cat("--- list:",n[i],"\n\n")
            show.all <- length(obj) <= 2*len
            if(show.all)
                print(obj)
            else {
                print(head(obj,len))
                cat("...\n")
                if(is.null(names(obj))) {
                    offset <- length(obj)-len
                    for(txt in capture.output(print(tail(obj,len)),file=NULL)) {
                        rx <- regexpr("^ *\\[\\[(\\d+)\\]\\](.*)$",txt,perl=TRUE)
                        if(rx > 0) {
                            nr <- sprintf(
                                "%d",
                                offset
                                + safen(substr(txt,
                                               start=attr(rx,"capture.start")[1],
                                               stop=attr(rx,"capture.start")[1]+attr(rx,"capture.length")-1)))
                            cat("[[",nr,"]]",
                                substr(txt,
                                       start=attr(rx,"capture.start")[2],
                                       stop=nchar(txt)),
                                "\n",sep="")
                        } else
                            cat(txt,"\n")
                    }
                } else
                    print(tail(obj,len))
            }
        } else if(is.vector(obj)) {
            cat("--- vector:",n[i],"\n\n")
            show.all <- length(obj) <= 2*len
            if(show.all)
                print(obj)
            else {
                print(head(obj,len))
                cat("...\n")
                if(is.null(names(obj))) {
                    offset <- length(obj)-len
                    for(txt in capture.output(print(tail(obj,len)),file=NULL)) {
                        rx <- regexpr("^ *\\[(\\d+)\\](.+)$",txt,perl=TRUE)
                        nr <- sprintf(
                            "%d",
                            offset
                            + safen(substr(txt,
                                           start=attr(rx,"capture.start")[1],
                                           stop=attr(rx,"capture.start")[1]+attr(rx,"capture.length")-1)))
                        cat("[",nr,"]",substr(txt,start=attr(rx,"capture.start")[2],stop=nchar(txt)),"\n",sep="")
                    }
                } else
                    print(tail(obj,len))
            }
        } else
            cat("Unsupported data type:",class(obj),"\n")
    }
    for(j in rep(0,post))
        cat("\n")
}
