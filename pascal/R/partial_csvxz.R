#' Read section of a (compressed) csv file
#'
#' The \code{read_partial} function reads part of a (possibly
#' compressed) csv file. The line range to read is passed with the
#' \code{lines} argument. \code{read_filtered} is similar but accepts
#' a \code{condition} that selects the lines to be read based on their
#' content. \code{guess_decompressor} suggests a command to decompress
#' the file to be read.
#'
#' @param file file name
#'
#' @param lines line interval to read. The end of the interval may be
#'     \code{NA}, in which case the file is read until the end.
#'
#' @param condition condition specifying which lines are to be
#'     read. Internally, it is passed to \code{awk}. The condition
#'     can reference data columns by their name. Due to the simplified
#'     parsing, fields must be separated by at least one space from
#'     operators. For example, \code{I == 7} will work but not
#'     \code{I==7}. Also, the logical operators must conform to awk's
#'     standards; specifically, one should use the double operators
#'     and not the vectorized counterparts as in \code{R},
#'     e.g. \code{&&} will work but \code{&} will not. See
#'     \url{https://www.gnu.org/software/gawk/manual/html_node/Boolean-Ops.html#Boolean-Ops}
#'     for details
#'
#' @param decompressor command to decompress the file. By default, the
#'     decompressor is chosen based on the file ending (.xz, .gz,
#'     .bz2, .zip are currently supported). Parallelized versions
#'     (\code{pigz} and \code{pbzip2}) are chosen if installed.
#'
#' @param debug logical (default FALSE). If TRUE, the command executed
#'     is displayed. Useful for debugging
#'
#' @param sep character separating fields (defaults to \code{,})
#'
#' @param ... additional arguments passed to \code{read.csv}.
#'
#' @return \code{data.frame} with file contents
#'
#' @importFrom utils read.csv
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname read_partial
#' @export
read_partial <- function(file,
                                lines=c(1,NA),
                                decompressor=NULL,
                                debug=FALSE,
                             ...) {
    if (is.null(decompressor))
        decompressor <- guess_decompressor(file)

    cmd <- sprintf("%s %s | awk 'NR==1 || NR>%d {print; %s}'",
                   decompressor,
                   file,
                   lines[1],
                   if (is.na(lines[2])) {
                       ""
                   } else {
                       sprintf("if(NR>%d) exit",lines[2])
                   })
    if(debug)
        cat("Command executed:",cmd,"\n")
    read.csv(pipe(cmd), ...)
}

#' @rdname read_partial
#' @export
read_filtered <- function(file,
                          condition,
                          decompressor=NULL,
                          sep=",",
                          debug=FALSE,
                          ...) {
    if (is.null(decompressor))
        decompressor <- guess_decompressor(file)

    hdr  <- unlist(
        strsplit(system(sprintf("%s %s | head -1", decompressor, file),
                        intern=TRUE),
                 sep))
    cond <- unlist(strsplit(condition," +", perl=TRUE))
    idx <- match(cond, hdr)
    awk <- paste(
        sapply(seq_along(idx),
               function(i) {
                   ifelse(is.na(idx[i]),
                          cond[i],
                          sprintf("$%d",idx[i]))
               }),
        collapse=" ")
    awk <- sprintf("BEGIN{FS=\"%s\"}{if((NR==1) || (%s)){ print; }}",sep, awk)
    cmd <- sprintf("%s %s | awk '%s'", decompressor, file, awk)
    if(debug)
        cat("Command executed:",cmd,"\n")
    read.csv(pipe(cmd), sep=sep, ...)
}


#' @rdname read_partial
#' @export
guess_decompressor <- function(file) {
    if (grepl("\\.xz$",file,ignore.case=TRUE))
        return("xz -dc --threads=0")
    if (grepl("\\.gz$",file,ignore.case=TRUE)) {
        if(system2("which","pigz",stdout=FALSE)==0)
            return("pigz -dc")
        else
            return("gzip -dc")
    }
    if (grepl("\\.bz2$",file,ignore.case=TRUE)) {
        if(system2("which","pbzip2",stdout=FALSE)==0)
            return("pbzip2 -dc")
        else
            return("bzip2 -dc")
    }
    if (grepl("\\.zip$",file,ignore.case=TRUE)) {
        return("unzip -p")
    }
    return("cat")
}
