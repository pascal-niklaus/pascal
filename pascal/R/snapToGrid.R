#' Snap to grid
#'
#' Applies a 2D affine transformation to a set of points (x,y) so that these
#' are places on an orthogonal grid with unit spacing.
#'
#' An optimal combination of rotation, translation, and scaling is
#' determined so that points are places as close as possible to
#' orthogonal grid points.  The fit criterion are minimized
#' least-square distances from these grid points.  An initial estimate
#' of the grid spacing needs to be provided, and this estimate is
#' adjusted by up to plus or minus 30\% in the fitting process.
#'
#' The function uses a generalized simulated annealing function from
#' library \code{GenSA} which is not fast but yields good results.
#' 
#' @param d list with elements x and y
#' 
#' @param theta initial clockwise rotation (default 0)
#' 
#' @param sz estimate of grid spacing (default 1)
#' 
#' @param which specify which results to return. Can be either "points" or "all".
#' 
#' @return list with transformed points (which == "points"), or list
#'     of transformation parameters, transformation matrix, and points
#'     (which == "all")
#'
#' @examples
#' # create gridded data with noise
#' d <- expand.grid(1:4,1:4) + matrix(runif(16, min=-.2, max=.2), ncol=2)
#' names(d) <- c("x","y")
#' 
#' # rotate, translate, and stretch grid
#' d <- apply2D(rotate2D(pi/10)%*%translate2D(.3,-.1)%*%scale2D(1.5,1.5),d)
#' 
#' # re-orient data on grid
#' d2 <- snapToGrid(d,theta=0,sz=1.4)
#'
#' # visualize results
#' par(mfrow=c(1,2))
#' plot(d$x,d$y,asp=1,main="original")
#' plot(d2$x,d2$y,asp=1, main="adjusted")
#' for(i in 0:5) { abline(h=i,col="gray"); abline(v=i, col="gray") } 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
snapToGrid <- function(d,theta=0, sz=1, which="points")
{
    if(!requireNamespace("GenSA"))
        stop("Library GenSA seems not to be available.")
    ## pre-scale 
    d$x <- d$x/sz
    d$y <- d$y/sz
    
    ## parameters: theta, shiftx, shifty, scale
    costFun <- function(par,data) {
        tmp <- apply2D(m = rotate2D(par[1]) %*% translate2D(par[2],par[3]) %*% scale2D(par[4],par[4]),
                      x = data)
        tmp$x <- tmp$x - round(tmp$x)
        tmp$y <- tmp$y - round(tmp$y)
        sum(tmp$x^2 + tmp$y^2)
    }
    
    r <- GenSA::GenSA(par=c(theta,0,0,1/sz),
               data = d,
               fn = costFun,
               lower=c(-pi/5,-.5,-.5, 0.7),
               upper=c(+pi/5,+.5,+.5, 1.3))
    m <- rotate2D(r$par[1]) %*% translate2D(r$par[2],r$par[3]) %*% scale2D(r$par[4],r$par[4])


    if(which=="points")
        apply2D(m,x=d)
    else 
        list(points = apply2D(m,x=d),
            params=list(theta=r$par[1],xshift=r$par[2],yshift=r$par[3],spacing=sz/r$par[4]),
            matrix = m)  
}

