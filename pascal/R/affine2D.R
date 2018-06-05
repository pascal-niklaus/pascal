#' Matrices for affince 2D transformations
#' 
#' Create 3 x 3 matrices for affine transformations
#'  
#' @param x amount to translate along x axis
#' 
#' @param y amount to translate along y axis
#'
#' @param kx scale factor along x axis
#' 
#' @param ky scale factor along y axis
#'
#' @param theta angle to rotate, clock-wise
#' 
#' @param phi angle to shear, away from other axis in direction of 1st quadrant
#' 
#' @return 3 x 3 transformation matrix
#' 
#' @examples
#' p1 <- polycircle(0,0,rx=3,ry=1,pos="BL")
#' plot(NULL,xlim=c(-5,5),ylim=c(-5,5),asp=1,xlab="",ylab="")
#' polygon(p1,border="red")
#' m <- scale2D(1.5) %*% rotate2D(pi/6) %*% translate2D(-3,-1)
#' p2 <- apply2D(m,p1)
#' polygon(p2,border="blue")
#' @seealso plot2D
#' @aliases affine2D
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname affine2D
#' @export
translate2D <- function(x,y)
{
    matrix(c(1,0,0,0,1,0,x,y,1),nrow=3)
}

#' @rdname affine2D
#' @export
scale2D <- function(kx,ky=kx)
{
    matrix(c(kx,0,0,0,ky,0,0,0,1),nrow=3)
}

#' @rdname affine2D
#' @export
rotate2D <- function(theta)
{
    matrix(c(cos(theta),-sin(theta),0,sin(theta),cos(theta),0,0,0,1),nrow=3)
}

#' @rdname affine2D
#' @export
shearx2D <- function(phi)
{
    matrix(c(1,0,0,tan(phi),1,0,0,0,1),nrow=3)
}

#' @rdname affine2D
#' @export
sheary2D <- function(phi)
{
    matrix(c(1,tan(phi),0,0,1,0,0,0,1),nrow=3)
}

#' @rdname affine2D
#' @export
reflectxy2D <- function()
{
    matrix(c(-1,0,0,0,-1,0,0,0,1),nrow=3)
}

#' @rdname affine2D
#' @export
reflectx2D <- function()
{
    matrix(c(1,0,0,0,-1,0,0,0,1),nrow=3)
}

#' @rdname affine2D
#' @export
reflecty2D <- function()
{
    matrix(c(-1,0,0,0,1,0,0,0,1),nrow=3)
}

#' @rdname affine2D
#' @export
identity2D <- function()
{
    diag(nrow=3)
}

#' Apply affine 2D transformation to (x,y) data
#' 
#' Transform (x,y) data passed either as separate vectors x and y or as list with
#' elements x and y.
#'  
#' @param x vector of x coordinates, or list with elements x and y when parameter y is NULL
#' 
#' @param y vector of y coordinates, or NULL when x is a list with elements x and y
#'
#' @param m transformation matrix
#' 
#' @return list with elements x and y
#' 
#' @examples
#' d <- list(x=1:4,y=rep(0,4))
#' plot(d,xlim=c(0,5),ylim=c(-1,4),asp=1)
#' d2 <- apply2D(rotate2D(-pi/6)%*%scale2D(1.5,1.5), d)
#' points(d2,col="red")
#' ## none so far...
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
apply2D <- function(m,x,y=NULL)
{
    tmp <- if(is.null(y)) 
               m %*% matrix(c(x$x,x$y,rep(1,length(x$x))),nrow=3,byrow=TRUE) 
           else 
               m %*% matrix(c(x,y,rep(1,length(x))),nrow=3,byrow=TRUE) 
    list(x=tmp[1,],y=tmp[2,])
}
