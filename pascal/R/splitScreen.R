#' Parametric interface to split.screen
#'
#' I use base graphics a lot, including `split.screen` to combined
#' multiple figures in a way not possible with this flexibilty with
#' `par(mfrow=...)`.  However, setting up the matrix for
#' `split.screen` is cumbersome.
#'
#' This function provides a parametric interface to `split.screen` for
#' regularly spaced and sized panels. The parameters are self-explanatory.
#'
#' @param nx,ny number of panels in horizontal and vertical direction
#'
#' @param gapx,gapy gap between panels in horizontal and vertical direction
#'
#' @param topy,bottomy,leftx,rightx extent of the four margins
#'
#' @param byrow logical indicating whether the panels are arranged
#'     left to right first and then top to bottom (default,
#'     byrow=TRUE) or top to bottom first and then left to right
#'     (byrow=FALSE)
#'
#' @param addLeft,addRight,addBottom,addTop logicals indicating
#'     whether a 'padding' screen should be added at these borders. It
#'     can be used to place legends that span across panels, for
#'     example. These screens are added at the end of the regular
#'     screens, in order left, right, bottom and top (when enabled).
#'
#' @param debug logical. If true, `split.screen` is not called but the
#'     matrix with the panel dimensions returned
#'
#' @return The matrix passed to `split.screen`
#'
#' @seealso \code{\link{split.screen}}
#' @importFrom graphics split.screen
#' @examples
#' library(pascal)
#' splitScreen(nx=3,ny=2,topy=.2)
#' ## a not very useful example...
#' for(i in 1:6) {
#'   screen(i)
#'   par(mai=c(0,0,0,0))
#'   plot(rnorm(10))
#' }
#' close.screen()
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
splitScreen <- function(nx=2, ny=1,
                        gapx=0, gapy=0,
                        topy=0, bottomy=0.2,
                        leftx=.2, rightx=0,
                        byrow=TRUE, debug=FALSE,
                        addLeft=FALSE,
                        addRight=FALSE,
                        addBottom=FALSE,
                        addTop=FALSE)
{
    w <- (1-leftx-rightx-(nx-1)*gapx)/nx
    h <- (1-topy-bottomy-(ny-1)*gapy)/ny

    m <- matrix(NA,nrow=nx*ny,ncol=4)
    for(x in 1:nx) {
        for(y in 1:ny) {
            i <- if(byrow) x+(y-1)*nx else y+(x-1)*ny
            m[i,] <- c(leftx+c(0,w)+(x-1)*(w+gapx),
                       bottomy+c(0,h)+(y-1)*(h+gapy))
        }
    }

    left  <- min(m[,1])
    right <- max(m[,2])
    top <- max(m[,4])
    bottom <- min(m[,3])
    if(addLeft)
        m <- rbind(m, c(0, left, bottom, top))
    if(addRight)
        m <- rbind(m, c(right, 1, bottom, top))
    if(addBottom)
        m <- rbind(m, c(left, right, 0, bottom))
    if(addTop)
        m <- rbind(m, c(left, right, top, 1))
    if(!debug)
        split.screen(m)
    invisible(m)
}


#' Provides an empty plot
#'
#' This function
#'
#' This function provides a parametric interface to `split.screen` for
#' regularly spaced and sized panels. The parameters are self-explanatory.
#'
#' @param xlim,ylim extents of plot. Defaults to c(0,1) for both axis.
#'
#' @param xlab,ylab axis labels (default: empty)
#'
#' @param xaxs,yaxs,xaxt,yaxt axes characteristics. Defaults to 'i'
#'     and 'n'. See `?par` for details.
#'
#' @param bty box type. Defaults to none ('n'; see `?par` for details)
#'
#' @param asp aspect ratio. Defaults to NA.
#'
#' @param xpd Defauts to NA. Check `?par` for details.
#'
#' @param ... Additional parameters passed to `plot`
#'
#' @seealso \code{\link{par}}, \code{\link{plot}}
#' @importFrom graphics plot
#' @examples
#' emptyPlot()
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
emptyPlot <- function(xlim=c(0,1),ylim=c(0,1),
                     asp=NA, xpd=NA,
                     xlab="",ylab="",bty="n",
                     xaxs="i",yaxs="i",
                     xaxt="n",yaxt="n",
                     ...) {
    plot(NA, NA,
         xlim=xlim, ylim=ylim,
         asp=asp, xpd=xpd, bty=bty,
         xlab=xlab, ylab=ylab,
         xaxs=xaxs, yaxs=yaxs,
         xaxt=xaxt, yaxt=yaxt,
         ...)
}
