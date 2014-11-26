#' Get objects of a certain class
#'
#' This function returns the objects defined in the current frame that
#' are of a given class (by default functions)
#' @param cls class type (default \code{function})
#' @return list of objects of the specified class
#' 
#' @examples
#' library(pascal)
#' a<-1
#' b <- function(x) { x }
#' getObjects("numeric");
#' getObjects("function");
#' ## remove all objects that are not functions
#' ls()
#' rm(list=setdiff(ls(),getObjects()));
#' ls()  
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
getObjects <- function(cls="function") {
  x<-ls(parent.frame());    
  xClass <- as.character(sapply(x,function(x) { class(get(x,envir=parent.frame())); }));
  x[xClass==cls];  
}
 
