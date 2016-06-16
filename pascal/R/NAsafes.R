#' Convenience functions excluding NAs
#'
#' These functions call standard functions with the extra argument na.rm=TRUE.
#' These are shortcuts that generally could be replaced by passing anonymous functions
#' as arguments (see examples below).
#'
#' @param x A data vector possibly containg NAs
#' @return The result of the respective summary function, called with \code{na.rm=TRUE}
#' @examples
#' #
#' testDF<-data.frame(by=rep(LETTERS[1:2],each=4),y=c(1,3,2,9,4,4,NA,1))
#' aggregate(testDF$y,by=list(by=testDF$by),FUN=mean)
#' #    by    x
#' #  1  A 3.75
#' #  2  B   NA
#' aggregate(testDF$y,by=list(by=testDF$by),FUN=mean_NAsafe)
#' #    by    x
#' #  1  A 3.75
#' #  2  B 3.00
#' aggregate(testDF$y,by=list(by=testDF$by),function(x) { mean(x,na.rm=TRUE) } )
#' @seealso \code{\link{mean}} \code{\link{var}} \code{\link{se}} \code{\link{sd}} \code{\link{median}} \code{\link{min}} \code{\link{max}}  
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @importFrom stats var sd median
#' @rdname NAsafes
#' @export
mean_NAsafe <- function(x) { return(mean(x,na.rm=T)) }

#' @rdname NAsafes
#' @export
se_NAsafe <- function(x) { return(se(x,na.rm=T)) }

#' @rdname NAsafes
#' @export
var_NAsafe <- function(x) { return(var(x,na.rm=T)) }

#' @rdname NAsafes
#' @export
sd_NAsafe <- function(x) { return(sd(x,na.rm=T)) }

#' @rdname NAsafes
#' @export
median_NAsafe <- function(x) { return(median(x,na.rm=T)) }

#' @rdname NAsafes
#' @export
min_NAsafe <- function(x) { return(min(x,na.rm=T)) }

#' @rdname NAsafes
#' @export
max_NAsafe <- function(x) { return(max(x,na.rm=T)) }
