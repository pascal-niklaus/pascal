#' Histogram panel for \code{pairs}
#'
#' \code{pairs} offers panel functions, but the one to display a
#' histogram in the diagonal elements is not predefined (althouth
#' there is an example in the help).
#'
#' This function implements such a function, and also produced
#' reasonable histograms for log-transformed scales.
#'
#' @param x values passed from \code{\link{pairs}}
#'
#' @param ...  additional arguments passed to drawing command
#'     (\code{\link{rect}})
#'
#' @seealso \code{\link{pairs}}
#' @examples
#' data(iris, package = "datasets")
#' iris[, 1:4]
#'
#' ##    Sepal.Length Sepal.Width Petal.Length Petal.Width
#' ## 1           5.1         3.5          1.4         0.2
#' ## 2           4.9         3.0          1.4         0.2
#' ## 3           4.7         3.2          1.3         0.2
#' ## 4           4.6         3.1          1.5         0.2
#'
#' pairs(iris[, 1:4], upper.panel = NULL, diag.panel = panel.hist)
#'
#' pairs(iris[, 1:4], log = 1:4, upper.panel = NULL, diag.panel = panel.hist)
#'
#' ## Probably not what you want, but possible:
#' pairs(iris[, 1:2], log = 1:2, diag.panel = function(x) panel.hist(x, col = "green", border = "magenta", lwd = 2))
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @import datasets
#' @importFrom grDevices rgb
#' @importFrom graphics abline hist rect

#' @export
panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par("usr" = usr))
    ylog <- par("ylog")
    on.exit(par("ylog" = ylog))
    par(ylog = FALSE)
    xlog <- par("xlog")
    par(usr = c(usr[1:2], 0, 1.5))
    if (xlog) {
        his <- hist(log(x), plot = FALSE)
        breaks <- exp(his$breaks)
    } else {
        his <- hist(x, plot = FALSE)
        breaks <- his$breaks
    }
    nB <- length(breaks)
    y <- his$counts
    y <- y / max(y)
    args <- list(...)
    if (is.null(args$col)) {
        args$col <- rgb(0, 1, 1, alpha = 0.5)
    }
    args$xleft <- breaks[-nB]
    args$xright <- breaks[-1]
    args$ybottom <- 0
    args$ytop <- y
    do.call(rect, args)
}

#' panel with regression line for \code{pairs}
#'
#' \code{pairs} offers panel functions, but the one to display a
#' regression line in the off-diagonal elements is not predefined.
#'
#' This function implements such a function, allowing to chose between
#' OLS regression (the default) and main axis regression. Internally,
#' it calls \code{lmodel2}.
#'
#' @param x values passed from \code{\link{pairs}}
#'
#' @param y values passed from \code{\link{pairs}}
#'
#' @param type the type of regression to be performed. Default is
#'     \code{'OLS'}. See \code{lmodel2} for other options.
#'
#' @param col,pch,cex symbol color, type, and size
#'
#' @param line.col,line.lwd,line.lty regression line color, width, and
#'     type. All arguments can be vectors of length two, in which case
#'     the 2nd element is chosen when the regression slope is
#'     statistically significant at P<0.05.
#'
#' @param ... additional arguments passed to \code{lmodel2}.
#'
#' @seealso \code{\link{pairs}}, \code{lmodel2}
#' @examples
#' data(iris, package = "datasets")
#'
#' pairs(iris[, 1:4],
#'     diag.panel = panel.hist,
#'     lower.panel = panel.line
#' )
#'
#' pairs(iris[,1:4],
#'       diag.panel=panel.hist,
#'       lower.panel=function(x,y) {
#'           panel.line(x, y, range.y = "relative", range.x = "relative", type = "RMA",
#'                      col = "green", cex = 2,
#'                      line.col = c("red","gray"),
#'                      line.lwd = c(2, 1),
#'                      line.lty = c(1, 3))
#'       }
#' )
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @import datasets
#' @export
panel.line <- function(x, y,
                       type = "OLS",
                       col = rgb(1, 0, 0, alpha = 0.5),
                       pch = 16,
                       cex = 1,
                       line.col = "blue",
                       line.lwd = 2,
                       line.lty = c(1, 2),
                       ...) {
    requireNamespace("lmodel2")

    if (par("xlog") || par("ylog")) {
        warning("log scales not supported")
        return()
    }

    line.col <- rep(line.col, 2)[1:2]
    line.lwd <- rep(line.lwd, 2)[1:2]
    line.lty <- rep(line.lty, 2)[1:2]

    points(x, y, col = col, pch = pch, cex = cex)

    m <- suppressMessages(lmodel2::lmodel2(y ~ x, ...))
    pars <- m$regression.results[m$regression.results$Method == type, ]
    icpt <- pars[, "Intercept"]
    slope <- pars[, "Slope"]
    ci <- m$confidence.intervals[
                m$confidence.intervals$Method == "OLS",
                c("2.5%-Slope", "97.5%-Slope")]
    significant <- (prod(sign(ci)) < 0)
    abline(icpt, slope,
        lty = line.lty[significant + 1],
        col = line.col[significant + 1],
        lwd = line.lwd[significant + 1]
    )
}
