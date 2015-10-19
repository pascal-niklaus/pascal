#' Extract distance from "dist" object
#'
#' Objects of class \code{dist} are triangular
#' matrices. \code{getDist} extracts distances between specific pairs
#' of items.
#'
#' There is, as of today (July 2015), no simple way to extract
#' individual distances from \code{dist} objects. The present
#' function indices the \code{dist} object, given two labels
#' specifying the item to extract. 
#'
#' @param d \code{dist} object
#' @param i item i. Must be either a label or an integer index; can be a vector.
#' @param j item j, as described for i.
#' @return distance(s) between \code{i} and \code{j}
#' @seealso \code{\link{dist}} 
#' @examples
#' data(USJudgeRatings)
#' d <- dist(USJudgeRatings)
#' getDist(d,c("PASKEY,L.","SHEA,D.M.","LEVINE,I."),rep("LEVINE,I.",3))
#' ## [1] 1.56 2.35 0.00
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
getDist <- function(d,i,j) {
    if(! ("dist" %in% class(d)) )
        stop("Object d is not of class \"dist\"")
    nm <- attr(d,"Labels")
    n <- attr(d,"Size")
    ii <- if(class(i) == "numeric")
              i
          else
              match(i,nm)
    jj <- if(class(j) == "numeric")
              j
          else
              match(j,nm)
    if(any(is.na(ii)))
        stop("One of the indices not found: ",i[is.na(ii)])
    if(any(is.na(jj)))
        stop("One of the indices not found: ",j[is.na(jj)])
    tmp <- pmin(ii,jj)
    jj <- pmax(ii,jj)
    ii <- tmp
    ifelse(ii==jj,
           0,
           d[n*(ii-1) - ii*(ii-1)/2 + jj-ii])
}
