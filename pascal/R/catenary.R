#' Catenary curves
#'
#' \code{catenary} constructs a catenary given some
#' parameter. \code{catenary_normal} returns vectors normal to the
#' secant between the catenary's end points to the catenary.
#'
#' A catenary has the general form \eqn{y = a cosh(x/a)}, with shape
#' parameter \code{a}. These functions re-parameterize this general
#' form in terms of y values \code{y1} and \code{y2} at the left and
#' right end of the caternary, and the horizontal distance between end
#' points \code{dx}. As alternative to the shape parameter \code{a},
#' the maximum sag \code{smax} along the catenary can be passed (in
#' other words: the maximum vertical distance between the direct line
#' connecting the end points and the catenary).
#'
#' For simplicity of implementation, the re-parameterization of the
#' catenary in terms of maximum sag \code{smax} instead of \code{a} is
#' done with an interative search using the Newton
#' algorithm. Similarly, the normals to the secant are determined iteratively.
#'
#' @param x location at which y is to be determined. Should be between
#'     \code{0} and \code{dx}.
#' @param y1,y2,dx y values and horizontal distance between end points
#'     of the caternary. The end points thus are (0,y1) and (dx,y2).
#' @param a shape parameter
#' @param smax alternative shape parameter indicating the maximum sag
#'
#' @return \code{catenary} returns a list with coordinates along the
#'     caternary (\code{x}, \code{y}), the lowest point (\code{xmin},
#'     \code{ymin}), the point of maximum sag (\code{xc}, \code{yc}),
#'     the shape parameter \code{a}, and the maximum sag \code{smax}.
#'     \code{caternary_normal} returns points along the secant
#'     (\code{xline},\code{yline}), and a second set of points on the
#'     caternary (\code{x},\code{y}). The connections between both
#'     point sets are normal to the secant. A last element \code{dist}
#'     returns the length of these connections.
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname catenary
#' @examples
#' \dontrun{
#' dx <- 2
#' y1 <- 2
#' y2 <- 3
#' r <- catenary(x=seq(0, dx, length=30), dx=dx, y1=y1, y2=y2, smax=.7)
#' plot(r, type="l", asp=1, las=1, ylab="y", xlab="x")
#' lines(c(0, dx), c(y1, y2), lty=2, col="gray")
#' abline(h=r$ymin, col="red")
#' text(1.5, r$ymin, pos=3, "ymin", col="red")
#' abline(v=c(0, dx), col="#80808080")
#' points(r$xc, r$yc, col="blue")
#' arrows(r$xc, r$yc, r$xc, r$yc+r$smax, col="orange", length=.1, code=3)
#' text(r$xc, r$yc+0.5*r$smax, srt=90, pos=2, col="orange", "smax")
#'
#' # add the normals to the secant
#' r <- catenary_normal(x=seq(0, dx, length=10), dx=dx, y1=y1, y2=y2, smax=.7)
#' arrows(r$xline, r$yline, r$x,r$y, length=.1, col="blue")
#' }
#'
#' @importFrom stats nlm
#' @export
catenary <- function(x, a=NULL, dx=1, y1=0, y2=0, smax=NULL)
{
    stopifnot(is.null(a) || is.null(smax))
    if(is.null(a)) {
        ## determine a from smax. nlm sometimes does not do well... call it 3 times
        r <- list(estimate=dx)
        for(i in 1:3)
            r <- nlm(.cat_smax_fun, r$estimate, dx=dx, y1=y1, y2=y2, smax=smax)
        if(r$minimum > (smax/1e3)^2)
            stop("Could not accurately determine a from smax")
        a <- r$estimate
    }
    r <- .cat_helper(a=a, dx=dx, y1=y1, y2=y2)
    y <- a * cosh((x-r$xmin)/a)-a+r$ymin
    list(x=x, y=y, xmin=r$xmin, ymin=r$ymin, xc=r$xc, yc=r$yc, a=a, smax=r$smax)
}

#' @rdname catenary
#' @export
catenary_normal <- function(x, a=NULL, dx=1, y1=0, y2=0, smax=NULL)
{
    sec <- c(dx, y2-y1) / dx  # vector along secant, scaled for sanity
    r <- catenary(0, a=a, dx=dx, y1=y1, y2=y2, smax=smax)
    tmp <-
        lapply(x,
           function(xline) {
               yline <- y1 + (y2-y1)/dx * xline
               n <- nlm(.cat_norm_fun, p=xline, xline=xline, yline=yline,
                        a=r$a, xmin=r$xmin, ymin=r$ymin, sec=sec)
               xcat <- n$estimate
               ycat <- r$a*cosh((xcat-r$xmin)/r$a)-r$a+r$ymin
               c(xline,yline,xcat,ycat, vector.norm(c(xcat,ycat)-c(xline,yline)))
           })
    tmp <- lapply(1:5, function(i) sapply(tmp, function(a) a[i]))
    names(tmp)<-c("xline","yline","x","y","dist")
    tmp
}

######################################################################
###
### .cat_helper
###
### given a, dx, y1, y2, calculate (xmin,ymin), (xc,yc), and smax
###
.cat_helper <- function(a, ...) {
    d <- list(...)
    xmin <- d$dx/2 - a * asinh((d$y2-d$y1)/(2*a*sinh(d$dx/2/a)))
    ymin <- d$y1 - a*cosh(-xmin/a)+a
    xc <- xmin + a * asinh((d$y2-d$y1)/d$dx)
    yc <- a * cosh((xc-xmin)/a)-a+ymin
    smax <- (d$y2-d$y1)/d$dx*xc+d$y1-yc
    list(xmin=xmin, ymin=ymin, xc=xc, yc=yc, smax=smax)
}

######################################################################
###
### .cat_smax_fun
###
### function to find 'a' that given 'smax'
###
.cat_smax_fun <- function(a, ...) {
    smax <- list(...)$smax
    tmp <- .cat_helper(a, ...)
    (tmp$smax - smax)^2
}

######################################################################
###
### .catnorm_fun
###
### function to find x on catenary so that vector
### (xline,yline)-(xcat,ycat) is normal to the secant
###
.cat_norm_fun <- function(x, ...) {
    d <- list(...)
    psec <- c(d$xline,d$yline)
    pcat <- c(x, d$a*cosh((x-d$xmin)/d$a)-d$a+d$ymin)
    (sum(d$sec * (pcat-psec)))^2
}
