#' Plot error bars
#' 
#' Given a set of points (x,y), error bars in horizontal and/or vertical direction are plotted.
#'
#' @param x vector of x coordinates of data points to be fitted with error bars
#' @param y vector of y coordinates of data points to be fitted with error bars
#' @param yplus extention of y error bar in positive (upward) direction
#' @param yminus extention of y error bar in negative (downward) direction (this is a positive number)
#' @param xplus extention of x error bar in positive (rightward) direction 
#' @param xminus extention of x error bar in negative (leftward) direction (this is a positive number) 
#' @param xerr alternative notation for symmetric error bars: if given, then xplus and xminus are set to this value 
#' @param yerr alternative notation for symmetric error bars: if given, then yplus and yminus are set to this value 
#' @param cap width of the little lines at the end of the error bars in units of the width of the plot. Default is .015. 
#' @param add set to TRUE to add bars to an existing plot 
#' @param lty line type for error bars 
#' @param lwd line width for error bars 
#' @param ... extra arguments passed to \code{segments} , e.g. to colors
#' @examples
#' ## example: barplot
#' data(CO2, package="datasets")
#' CO2.aggr<-aggr(CO2,c("Treatment"),c("uptake=mean(uptake)","se.uptake=se(uptake)"))
#' x<-barplot(CO2.aggr$uptake,names.arg=CO2.aggr$Treatment,ylim=c(0,max(CO2.aggr$uptake+CO2.aggr$se.uptake)))
#' xy.errbar(x,CO2.aggr$uptake,yerr=CO2.aggr$se.uptake,add=TRUE)
#' 
#' ## example: x/y plot
#' CO2.aggr<-aggr(CO2,c("conc"),c("uptake=mean(uptake)","se.uptake=se(uptake)"))
#' xy.errbar(safen(CO2.aggr$conc),CO2.aggr$uptake,yerr=CO2.aggr$se.uptake)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @importFrom graphics plot segments points par
#' @export
xy.errbar <- function (x, y, yplus=NULL, yminus=NULL, xplus=NULL, xminus=NULL, yerr=NULL, xerr=NULL, cap = 0.015, 
                       lty = 1, lwd = 1, add=FALSE, ...) {    
    if (!add)
        plot(x, y, ...)

    xcoord <- par()$usr[1:2];
    ycoord <- par()$usr[3:4];

    xwidth <- diff(par()$usr[1:2]);
    ywidth <- diff(par()$usr[3:4]);

    x.scale <- xwidth/par()$pin[1];
    y.scale <- ywidth/par()$pin[2];

    xsmidge <- cap * (xcoord[2] - xcoord[1])/2;
    ysmidge <- xsmidge * y.scale/x.scale;    

    if(!is.null(yerr)) { yplus<-yerr; yminus<-yerr; };
    if(!is.null(xerr)) { xplus<-xerr; xminus<-xerr; };
    if(!is.null(yminus) & !is.null(yplus)) {
        segments(x, y-yminus, x, y+yplus, lty = lty, lwd = lwd, ...);        
        segments(x - xsmidge, y-yminus, x + xsmidge, y-yminus, lwd = lwd, ...);
        segments(x - xsmidge, y+yplus,  x + xsmidge, y+yplus,  lwd = lwd, ...);
    } else if(!is.null(yplus)) {
        segments(x, y, x, y+yplus, lty = lty, lwd = lwd, ...);                
        segments(x - xsmidge, y+yplus,  x + xsmidge, y+yplus,  lwd = lwd, ...);
    } else if(!is.null(yminus)) {
        segments(x, y-yminus, x, y, lty = lty, lwd = lwd, ...);        
        segments(x - xsmidge, y-yminus, x + xsmidge, y-yminus, lwd = lwd, ...);
    }
    if(!is.null(xminus)) {
        segments(x-xminus, y, x+xplus, y, lty = lty, lwd = lwd, ...);        
        segments(x - xminus, y - ysmidge, x - xminus, y + ysmidge, lwd = lwd, ...);
        segments(x + xplus,  y - ysmidge, x + xplus,  y + ysmidge, lwd = lwd, ...);
    }
    invisible();
}

