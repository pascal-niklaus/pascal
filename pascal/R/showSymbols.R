#' Show base plotting symbols
#'
#' This command produces a simple plot showing the symols
#' corresponding to \code{pch} values of 0 to 25. 
#' @seealso \code{\link{plot}} 
#' @seealso \code{\link{points}} 
#' @examples
#' showSymbols()
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @importFrom graphics points text plot
#' @export    
showSymbols <- function()
{
    plot(0,0,
         ty="n",bty="n",
         xaxt="n",yaxt="n",
         xlab="",ylab="",
         xlim=c(0,5),ylim=c(-.5,6),
         main="Symbols for pch values")
  i <- 0:25
  points( i %% 5, 5-floor(i/5),pch=i,cex=2)
  text( i %% 5, 5-floor(i/5),labels=i,pos=1,cex=1,col="red",offset=1)
}
