#' Progress bar
#'
#' This is a simple progress indicator that can be used in loops.
#'
#' The progress indicator is only active if sinks are inactive (see
#' \code{enableSink}), i.e. if output is shown on the screen. A
#' progress bar needs to be initialized with
#' \code{progressInit}. Then, one can either display it by repeatedly
#' calling \code{progressBar} with the current step as argument. This
#' re-draws the entire progress bar on every call. Alternatively, one
#' might call \code{progressStep0}, which draws the bar, and then call
#' \code{progressStep} for each step, which will draw single
#' characters that slowly fill up the pre-drawn bar. The latter also
#' works with parallelized execution using \code{mclappy}.
#'
#' (Implementation details: The challenge is for this to work with
#' parallel processing. I settled on a shared memory object (a
#' \code{big.matrix}) with a \code{boost.mutex} to ensure exclusive
#' access.  This does not work under Windows, but then \code{mclapply}
#' does not fork under Windows either.)
#'
#' @param steps The number of steps can be passed in different ways:
#'     1) directly as integer; 2) as vector, in which case the length
#'     of the vector determines the number of steps; 3) as
#'     \code{data.frame} or similar structure, in which case the
#'     extent in the first dimension determines the number of steps.
#' @param step Current step (integer ranging from 0 up to the maximum
#'     number of steps).
#' @param pre,post Text to draw before and after the bar. Defaults to
#'     '[' and ']'
#' @param spc,done characters indicating whether a step is not yet
#'     done (spc) or already completed (done). Default to '-' and '*'.
#' @param maxwidth maximum width of the bar on the screen. Defaults to
#'     60.
#' @seealso \code{\link{enableSink}}
#' @examples
#' # step-wise progress:
#' progressInit(80); progressStep0(); for(i in 1:80) { progressStep(); Sys.sleep(.02); };
#'
#' # With full bar redrawn for every step:
#' progressInit(80); for(i in 1:80) { progressBar(i); Sys.sleep(.02); };
#'
#' # same, but with percentage displayed
#' progressInit(80); for(i in 1:80) { progressPct(i); Sys.sleep(.02); };
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname progress
#' @export
progressInit <- function(steps = 10,
                         pre = "[",
                         post = "]",
                         done = "*",
                         spc = "-",
                         maxwidth = 60) {
    .progressInit()

    if (!is.null(dim(steps)))
        steps <- dim(steps)[1]
    else if (length(steps) > 1)
        steps <- length(steps)

    div <- ceiling(steps / maxwidth)
    nbar <- ceiling(steps / div)

    .localstuff$progress <- list(
        steps = steps,
        nbar = nbar,
        div = div,
        pre = pre,
        post = post,
        done = done,
        spc = spc
    )
}
#' @rdname progress
#' @export
progressBar <- function(step = 0) {
    if (.localstuff$sinkOn)
        return
    with(.localstuff$progress, {
        stp <- floor(step / div)
        cat(pre,
            paste(rep(done, stp), collapse = ""),
            paste(rep(spc, nbar - stp), collapse = ""),
            post,
            "\r",
            sep = ""
        )
    })
    if (.localstuff$progress$steps == step)
        cat("\n")
}

#' @rdname progress
#' @export
progressStep0 <- function() {
    .progressSetCounter(0)
    progressBar(0)
    cat(.localstuff$progress$pre)
}

#' @rdname progress
#' @export
progressStep <- function() {
    stp <- .progressIncCounter()
    if (.localstuff$sinkOn)
        return
    if (stp %% .localstuff$progress$div == 0)
        cat(.localstuff$progress$done)
    if (stp == .localstuff$progress$steps)
        cat(.localstuff$progress$post, "\n")
}

#' @rdname progress
#' @export
progressPct <- function(step = 0) {
    if (.localstuff$sinkOn)
        return
    with(.localstuff$progress, {
        pct <- sprintf("%3d%%", round(step / steps * 100))
    cat(pre, pct, post, "\r")
    })
    if (.localstuff$progress$steps == step)
        cat("\n")
}

######################################################################
###
### private functions

.progressInit <- function() {
    .localstuff$progress.do.lock <-
        (requireNamespace("synchronicity") &&
         requireNamespace("bigmemory"))

    if (.localstuff$progress.do.lock) {
        .localstuff$progress.counter <- bigmemory::big.matrix(1, 1)
        .localstuff$progress.counter[1, 1] <- 0
        .localstuff$progress.mutex <- synchronicity::boost.mutex()
    } else {
        .localstuff$progress.counter <- 0
    }
}

.progressIncCounter <- function() {
    if (.localstuff$progress.do.lock) {
        synchronicity::lock(.localstuff$progress.mutex)
        n <- (.localstuff$progress.counter[1, 1] <-
                  .localstuff$progress.counter[1, 1] + 1)
        synchronicity::unlock(.localstuff$progress.mutex)
    } else {
        n <- (.localstuff$progress.counter <-
                  .localstuff$progress.counter + 1)
    }
    n
}

.progressSetCounter <- function(n) {
    if (.localstuff$progress.do.lock) {
        synchronicity::lock(.localstuff$progress.mutex)
        .localstuff$progress.counter[1, 1] <- n
        synchronicity::unlock(.localstuff$progress.mutex)
    } else {
        .localstuff$progress.counter <- n
    }
}

.progressGetCounter <- function() {
    if (.localstuff$progress.do.lock) {
        synchronicity::lock(.localstuff$progress.mutex)
        n <- .localstuff$progress.counter[1, 1]
        synchronicity::unlock(.localstuff$progress.mutex)
    } else {
        n <- .localstuff$progress.counter
    }
    n
}
