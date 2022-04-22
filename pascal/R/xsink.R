#' Sink that can be enabled/disabled
#'
#' Often, one sinks results to a file. During debugging, this may be
#' disadvantageous because the output and important messages may be
#' invisible on the console.  Combining each sink command with a
#' separate \code{if(sinkEnabled) { ... }} appears cumbersome, and so
#' does commenting out the respective lines.  \code{Sink},
#' \code{enableSink}, and \code{enableSink} offer alternatives.
#' Essentially, the \code{Sink(...)} command is only effective if
#' enabled.
#'
#' @param enabled TRUE to enable and FALSE to disable sinking.
#'
#' @param ... parameters passed to \code{sink}
#'
#' @param width width of output when printing to sink. When the sink
#'     is closed, the original output is restored. Note that sinks
#'     cannot be nested.
#'
#' @return \code{checkSink()} will return TRUE if sinking is enabled,
#'     FALSE otherwise.
#'
#' @seealso \code{\link{sink}}
#'
#' @examples
#' enableSink()
#' # replacing the above line with enableSink(FALSE) or disableSink() will
#' # turn off the effect of the following Sink commands
#' Sink("file.txt")
#' # do some stuff here
#' Sink()
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname xsink
#' @export
Sink <- function(..., width=NA) {
    if(.localstuff$sinkOn) {
        a <- list(...)
        sink(...)
        if (length(a) == 0) {
            ## Sink() was closed, restore width if necessary
            if (! is.null(.localstuff$oldwidth)) {
                options(width = .localstuff$oldwidth)
                .localstuff$oldwidth <- NULL
            }
        } else {
            ## Sink() was opened, set width if necessary
            if (is.na(width)) {
                .localstuff$oldwidth <- NULL
            } else {
                .localstuff$oldwidth <- options()$width
                options(width = width)
            }
        }
    }
}

#' @rdname xsink
#' @export
enableSink <- function(enabled=TRUE) {
  .localstuff$sinkOn <- enabled
}

#' @rdname xsink
#' @export
disableSink <- function() {
  enableSink(FALSE)
}

#' @rdname xsink
#' @export
checkSink <- function() {
  .localstuff$sinkOn
}

enableSink(TRUE)
