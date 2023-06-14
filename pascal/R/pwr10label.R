#' Generate plotmath expressions to label plot axes
#'
#' \code{pwr10label} generates a vector of plotmath expressions with
#' labels in the form X.XX*10^{e}.
#'
#' @param x the label values
#'
#' @param ndec the number of decimals for the mantissa
#'
#' @param exp the decadal exponent, e.g. 7 for 10^7
#'
#' @param expOnly logical indicating whether only the exponent part
#'     should be produced, omitting the mantissa
#'
#' @return a vector of expressions that can be passed to
#'     e.g. \code{text} or \code{axis}.
#'
#' @examples
#' \dontrun{
#'
#' ## example 1: exponential part only
#'
#' xvec <- seq(0, 10, length=100)
#' plot(NULL, xlim=range(xvec), ylim=c(-1,3),
#'      xlab="x", ylab=expression(x^3), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
#' ylabs <- seq(-1, 3)
#' left.axis(at=ylabs, label=pwr10label(10^ylabs, expOnly=TRUE))
#' bottom.axis()
#' lines(xvec, log10(xvec^3), col="red", lwd=2)
#'
#' ## example 2: exponent and mantissa
#'
#' par(mar=c(4,7,1,6),cex.lab=1.5)
#' ylabs <- c(1, 2, 5) * rep(10^(0:3),each=3)
#' plot(NULL, xlim=range(xvec), ylim=log10(range(ylabs)),
#'      xlab="X", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
#' left.axis(at=log10(ylabs), label=pwr10label(ylabs, ndec=0))
#' right.axis(at=log10(ylabs), label=pwr10label(ylabs, ndec=2, exp=2))
#' mtext(expression(X^3), 2, 4, cex=par()$cex.lab)
#' bottom.axis()
#' lines(xvec, log10(xvec^3), col="red", lwd=2)
#' #' }
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
pwr10label <- function(x,
                       ndec = 1,
                       exp = floor(log10(abs(x))),
                       expOnly = FALSE) {
    n <- length(x)
    stopifnot(length(exp) %in% c(1, n))
    exp <- rep(exp, n / length(exp))

    res <- vector("expression", n)
    for (i in 1:n) {
        if (expOnly)
            res[i] <- substitute(
                expression(10^{e}),
                list(e = exp[i])
            )[2]
        else
            res[i] <- substitute(
                expression(x %.% 10^{e}),
                list(x = sprintf("%.*f", ndec, x[i] / 10^exp[i]),
                     e = exp[i])
            )[2]
    }
    res
}
