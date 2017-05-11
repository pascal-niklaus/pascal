## Notes:

## http://www.visiondummy.com/2014/04/draw-error-ellipse-representing-covariance-matrix/


#' Confidence ellipse
#'
#' Calculate a confidence ellipse for bivariate data
#'
#' This function is useful to calculate confidence ellipses and their
#' major axes into ordination plots. Data can be provided either as
#' set of (x,y) values in a data frame or matrix (argument
#' \code{data}), or as covariance matrix \code{cov} and mean x and y
#' values \code{center}. \code{level} specifies the confidence
#' interval to draw.
#'
#' @param data data frame or matrix with x and y values in first and
#'     second column
#' @param cov covariance matrix of bivariate data
#' @param center center of ellipsoid
#' @param level confidence interval to draw (default = 0.95)
#' @param npoints number of points returned for the confidence ellipse
#'     (default 100)
#' @return list with parameters specifying the confidence
#'     ellipse. In particular, \code{ellipse} is a list of coordinates
#'     delineating the ellipse, and \code{ma1} and \code{ma2} the
#'     end points of the major axes. \code{scale} are the standard
#'     deviations of x and y.  \code{center} is the center of the ellipsoid.
#'     \code{r} is the correlation of x and y.
#'    
#' @examples
#' p <- rnorm(100,sd=1)
#' d <- data.frame(x=30+p+rnorm(100,sd=1), y=20-.5*p+rnorm(100,sd=2))
#' e <- ellipse(d)
#' plot(d$x,d$y,asp=1)
#' lines(e$ellipse, col="red")
#' lines(e$ma1,col="red")
#' lines(e$ma2,col="red")
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @note Part of the code is taken and adapted from the \code{ellipse} package,
#'     for my own purpouses.
#' @keywords misc, utilities
#' @importFrom stats dist optimize qchisq
#' @export    
ellipse <-
    function (data = NULL,
              cov = NULL,
              center = NULL,
              level = 0.95, 
              npoints = 100) 
{
    calcpts <-
        function(avec) {
            matrix(c(tt * scl[1] * cos(avec + d/2) + center[1],
                     tt * scl[2] * cos(avec - d/2) + center[2]),
                   length(avec),2)
        }

    if(!is.null(data)) {
        cov = stats::cov(as.matrix(data))
        center = colMeans(as.matrix(data))
    } else {
        if(is.null(cov) || is.null(center))
            stop("Either bivariate data ('data') , ",
                 "or both covariance ('cov') and means of x and y ('center') have to be provided")
    }
    
    tt = sqrt(qchisq(level, 2))
    r <- cov[1, 2] 
    scl <- sqrt(c(cov[1,1], cov[2,2]))
    if (scl[1] > 0) r <- r/scl[1]
    if (scl[2] > 0) r <- r/scl[2]
    r <- min(max(r,-1),1)  
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    m <- matrix(c(tt * scl[1] * cos(a + d/2) + center[1],
                  tt * scl[2] * cos(a - d/2) + center[2]),
                npoints,2)

    ## the major axis could for sure be specified using the parameters above,
    ## but I did not have time to do that. Instead, I use a "dummy" method
    ## by searching for the largest and shortest connection through the center

    ## major axis 1    
    ma1 <-        
        optimize(
            function(avec) {
                dist(calcpts(c(avec,avec+pi)))                            
            },
            lower=0,
            upper=pi,
            maximum = TRUE)
    ma1 <- calcpts(ma1$maximum+c(0,pi))

    ## major axis 2    
    ma2 <-        
        optimize(
            function(avec) {
                dist(calcpts(c(avec,avec+pi)))                            
            },
            lower=0,
            upper=pi,
            maximum = FALSE)              
    ma2 <- calcpts(ma2$minimum+c(0,pi))

    ## check for non-convergence, indicated by non-orthogonal axes.    
    ## most likely this occurs for a perfect circle.
    v1 <- (ma1[1,]-ma1[2,]) 
    v2 <- (ma2[1,]-ma2[2,])
    if( abs( v1 %*% v2 ) / ( dist(v1) * dist(v2) ) > 0.001 ) {
        ma1 <- calcpts(c(0,pi))
        ma2 <- calcpts(c(0,pi)+pi/2)
    }
    
    list(r=r,
         d=d,
         center=center,
         scale=scl,
         ma1 = ma1,
         ma2 = ma2,
         ellipse=calcpts(a))         
}




#' Draw confidence ellipse
#'
#' Draw a confidence ellipse for bivariate datainto a bivariate plot
#'
#' This function draws a confidence ellipse and/or its major axes into
#' a bivariate plot. 
#'
#' @param data data frame or matrix with x and y values
#' @param columns columns that hold x and y data
#'     (default 1 and 2; column names can also be passed)
#' @param subset logical indicating a subset of the data
#' @param level confidence interval to draw (default = 0.67)
#' @param which which elements are to be drawn (1: ellipse, 2: main axes)
#' @param fill.ellipse color in which ellipse area is filled, or NA for no fill
#' @param col.ellipse color of ellipse contour, or NA for no contour
#' @param lwd.ellipse width for ellipse 
#' @param lty.ellipse line type of ellipse 
#' @param col.ma color of major axes 
#' @param lwd.ma line width for major axes
#' @param lty.ma line type of major axes
#' @param caps bar ends added to major axes (if any are plotted, 0 = no ends)
#'    
#' @examples
#' p <- rnorm(100,sd=1)
#' d <- data.frame(x=30+p+rnorm(100,sd=1), y=20-.5*p+rnorm(100,sd=2))
#' e <- ellipse(d)
#' plot(d$x,d$y,asp=1)
#' plotellipse(d,caps=.2,col.ma="red",lwd.ma=2,fill="#ffff0040") 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @note Part of the code is taken and adapted from the \code{ellipse} package,
#'     for my own purpouses.
#' @keywords misc, utilities
#' @importFrom graphics arrows
#' @export    
plotellipse <- function(data, columns=c(1,2), subset=NULL, level=.67,
                        which = c(1,2),
                        fill.ellipse=NA,   
                        col.ellipse="black", col.ma="black",
                        lwd.ellipse=1, lwd.ma=1,
                        lty.ellipse=1, lty.ma=1,
                        caps=0) {
    mf <- match.call()    
    mf[[1]] <- as.name("model.frame")
    mf[! names(mf) %in% c("","data","subset") ] <- NULL    
    tmp <- eval(mf,parent.frame())[,columns]
    ell <- ellipse(data=tmp,level=level)
    if(1 %in% which) 
        polygon(ell$ellipse,col=fill.ellipse,border=col.ellipse)
    if(2 %in% which) {
        ##lines(ell$ma1,col=col.ma,lwd=lwd.ma,lty=lty.ma)
        ##lines(ell$ma2,col=col.ma,lwd=lwd.ma,lty=lty.ma)
        arrows(ell$ma1[1,1],ell$ma1[1,2],ell$ma1[2,1],ell$ma1[2,2],code=3,length=caps,angle=90,
               lwd=lwd.ma,lty=lty.ma,col=col.ma)
        arrows(ell$ma2[1,1],ell$ma2[1,2],ell$ma2[2,1],ell$ma2[2,2],code=3,length=caps,angle=90,
               lwd=lwd.ma,lty=lty.ma,col=col.ma)
    }            
}
