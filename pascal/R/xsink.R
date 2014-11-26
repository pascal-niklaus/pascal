#' Sink that can be enabled/disabled 
#'
#' Often, one sinks results to a file. During debugging, this may be
#' disadvantageous because the output and important messages may be invisible on the console.
#' Combining each sink command with a separate \code{if(sinkEnabled) { ... }} appears cumbersome,
#' and so does commenting out the respective lines.
#' \code{Sink}, \code{enableSink}, and \code{enableSink} offer alternatives.
#' Essentially, the \code{Sink(...)} command is only effective if enabled.
#' 
#' @param enabled TRUE to enable and FALSE to disable sinking.
#' @param ... parameters passed to \code{sink}
#' @return \code{checkSink()} will return TRUE if sinking is enabled, FALSE otherwise.
#' @seealso \code{\link{sink}} 
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
Sink <- function(...) 
{
  if(.localstuff$sinkOn) sink(...);
}

#' @rdname xsink
#' @export
enableSink <- function(enabled=TRUE)
{
  .localstuff$sinkOn <- enabled;
}

#' @rdname xsink
#' @export
disableSink <- function() 
{
  enableSink(FALSE);
}

#' @rdname xsink
#' @export
checkSink <- function()
{
  .localstuff$sinkOn;
}

enableSink(TRUE);
