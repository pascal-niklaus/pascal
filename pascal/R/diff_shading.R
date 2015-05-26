.mypoly <- function(x,y,sgn,col=c("gray","gray"),density=c(20,NA),angle=c(45,0),lty=c(1,1),lwd=c(1,1)) {
  idx <- if(sgn>=0) 1 else 2;
  polygon(x,y,col=col[idx],density=if(is.na(density[idx])) NULL else density[idx],
          angle=angle[idx],lwd=lwd[idx],lty=lty[idx])
}

#' Shade difference between two lines
#'
#' This set of functions provides a way to shade the area between two lines,
#' with different patterns or colors depending on which line is on top.
#' 
#' In essence, a plot is started by calling \code{newpoly}. Then, \code{addpoly} is 
#' repeatedly called, passing the values \code{x} plus \code{y1} and \code{y2}. 
#' These calls must be in order of ascending \code{x}, i.e. 'left to right'. At the end,
#' the function \code{flushpoly} is called to draw the final polygons.
#' \code{addpoly} keeps accumulating coordinates until the two lines cross, in which case
#' the intersection is computed, the polygon plotted, and a new polygon started.
#' Data between repeated calls is passed as 'memory' in a list structure \code{mem}.
#' Parameters defining the plots can be passed to \code{newpoly} when this initial
#' structure is created.
#' 
#' @param mem list structure passing 'memory' parameters between calls. 
#'        Must not be modified by used.
#' @param y1 Y coordinate of first line
#' @param y2 Y coordinate of second line
#' @param x X coordinate of points (x,y1) and (x,y2)
#' @param col vector of 2, indicating color for y1>y2 and y1<y2
#' @param density vector of 2, indicating density of shading lines, in lines per inch, 
#'        for y1>y2 and y1<y2. NA indicates no shading
#' @param angle vector of 2, indicating hatching angle
#' @param lty vector of 2 with line type of polygons, for y1>y2 and y1<y2
#' @param lwd vector of 2 with line width of polygons and shading, for y1>y2 and y1<y2
#' @return list structure that should be passed as \code{mem} parameter to the next
#'         call of \code{addpoly} or \code{flushpoly}
#' @examples
#' par(mfrow=c(1,2))
#' d<-data.frame(x=seq(0,2*pi,len=20))
#' d$y1 <- sin(d$x)
#' d$y2 <- 1.5*cos(2*d$x)-.5
#' plot(rep(d$x,2),c(d$y1,d$y2))
#' mem <- newpoly()
#' for(i in 1:nrow(d)) 
#'   mem<-addpoly(mem,d$x[i],d$y1[i],d$y2[i])
#' mem<-flushpoly(mem)
#' plot(rep(d$x,2),c(d$y1,d$y2))
#' mem <- newpoly(col=c("red","blue"),density=NA,lwd=1,lty=0)
#' for(i in 1:nrow(d)) 
#'   mem<-addpoly(mem,d$x[i],d$y1[i],d$y2[i])
#' flushpoly(mem)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname diff_shading
#' @export
addpoly <- function (mem,x,y1,y2) {
  if(!is.null(mem$x)) {
    ## shift memory of last point
    mem$prevx <- mem$x;
    mem$prevy1 <- mem$y1;
    mem$prevy2 <- mem$y2;

    if((mem$sgn!=0) && (mem$sgn != sign(y2-y1))) { 
      ## sign changed, add intersection and plot
      xx <- c(mem$prevx,x);
      xy <- isect(xx,c(mem$prevy2,y2),xx,c(mem$prevy1,y1));
      mem$xtop <- append(mem$xtop,xy[1]);
      mem$xbot <- append(mem$xbot,xy[1]);
      mem$ytop <- append(mem$ytop,xy[2]);      
      mem$ybot <- append(mem$ybot,xy[2]);      
      .mypoly(x=c(mem$xtop,rev(mem$xbot)),y=c(mem$ytop,rev(mem$ybot)),sgn=mem$sgn,
              col=mem$col,density=mem$density,angle=mem$angle,lty=mem$lty,lwd=mem$lwd)
      ## start a new memory (point at intersection)
      mem$xtop <- mem$xbot <- xy[1];
      mem$ytop <- mem$ybot <- xy[2];
    }
  }

  ## add two points
  mem$ytop <- append(mem$ytop,max(y1,y2));
  mem$ybot <- append(mem$ybot,min(y1,y2));
  mem$xtop <- append(mem$xtop,x);
  mem$xbot <- append(mem$xbot,x);
  mem$sgn  <- sign(y2-y1);

  ## store values for memory
  mem$y2 <- y2;
  mem$x <- x;
  mem$y1 <- y1;

  ## return memory
  invisible(mem);
}

#' @rdname diff_shading
#' @export
flushpoly <- function(mem)
{  
  x<-c(mem$xbot,mem$xtop);
  if(!is.null(x) && diff(range(x))>0) {
    .mypoly(x=c(mem$xtop,rev(mem$xbot)),y=c(mem$ytop,rev(mem$ybot)),sgn=mem$sgn,
            col=mem$col,density=mem$density,angle=mem$angle,lty=mem$lty,lwd=mem$lwd)
  }
  invisible(list(col=mem$col, angle=mem$angle, density=mem$density, lty=mem$lty, lwd=mem$lwd))
}

#' @rdname diff_shading
#' @export
newpoly <- function(col=NULL,angle=NULL,density=NULL,lwd=NULL,lty=NULL)
{
  mem <- list(col=c("gray","gray"),
              angle=c(45,45),
              density=c(20,NA),
              lty=rep(1,2),
              lwd=rep(1,2))
  if(!is.null(col)) {
    mem$col <- rep(col,2)[1:2];
  }
  if(!is.null(angle)) {
    mem$angle <- rep(angle,2)[1:2];
  }
  if(!is.null(density)) {
    mem$density <- rep(density,2)[1:2];
  }
  if(!is.null(lwd)) {
    mem$lwd <- rep(lwd,2)[1:2];
  }
  if(!is.null(lty)) {
    mem$lty <- rep(lty,2)[1:2];
  }
  invisible(mem);
}
