#' Escape characters in strings
#'
#' Replace selected characters in vectors, lists, or matrices by \\xxxx, where xxxx is their hexadecimal representation.
#' Note that the backslash is always escaped, since it is used as escape character.
#'
#' @param x Vector, list or matrix of character strings
#' @param to.escape String containing the characters to escape (default ':') 
#' @return Vector, list or matrix of string with the corresponding characters escaped
#' @examples
#' r<-escape(list("A:B","C:D"))
#' r
#' ## [[1]]
#' ## [1] "A\\003aB"
#' ## 
#' ## [[2]]
#' ## [1] "C\\003aD"
#' unescape(r)
#' ## [[1]]
#' ## [1] "A:B"
#' ## 
#' ## [[2]]
#' ## [1] "C:D"
#' @seealso \code{\link{unescape}}
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export 
escape <- function(x, to.escape=":")
{
  if(is.list(x)) {
    lapply(x,escape)
  } else if(is.matrix(x)) {
    apply(x,1:2,escape)
  } else if(is.vector(x) && length(x)>1) {
    r <- sapply(x,escape)
    names(r) <- names(x)
    r
  } else {  
    to.escape<-setdiff(unlist(strsplit(to.escape,"")),"\\")
    replacement <- sapply(to.escape, function(to.escape) { sprintf("\\\\%04x",.hexjoin(as.numeric(sapply(to.escape,charToRaw)))) });
    x<-gsub("\\\\",sprintf("\\\\%04x",as.numeric(charToRaw("\\"))),x);
    for(i in seq(along=to.escape))
      x<-gsub(to.escape[i],replacement[i],x);            
    x;
  }
}

#' Unescape special sequences in strings
#'
#' This is the inverse function of escape.
#'
#' @param x Vector, list or matrix of character strings with escaped special characters
#' @return Vector, list or matrix of strings with the corresponding sequences unescaped
#' @examples
#' r<-escape(list("A:B","C:D"))
#' r
#' ## [[1]]
#' ## [1] "A\\003aB"
#' ## 
#' ## [[2]]
#' ## [1] "C\\003aD"
#' unescape(r)
#' ## [[1]]
#' ## [1] "A:B"
#' ## 
#' ## [[2]]
#' ## [1] "C:D"
#' @seealso \code{\link{escape}}
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export 
unescape <- function(x)
{
  if(is.list(x)) {
    lapply(x,unescape)
  } else if(is.matrix(x)) {
    apply(x,1:2,unescape)
  } else if(is.vector(x) && length(x)>1) {
    r<-sapply(x,unescape)
    names(r) <- names(x)
    r;
  } else {  
    m<-gregexpr("\\\\[0-9a-fA-F]{4}",x)
    if(m[[1]][1]>0) {
      vals <- strtoi(gsub("\\\\","",unlist(regmatches(x,m))),16);
      r<-sapply(vals,function(v) rawToChar(as.raw(.hexsplit(v))));      
      regmatches(x,m)<-list(r);
    }
    x;
  }
}

# strip.dimnames<-function(x) 
# {
#   dimnames(x)<-NULL;
#   x
# }

.hexjoin <- function(x) {
  r<-0;
  for(i in x) 
    r<-bitwShiftL(r,8)+i;
  r  
}

.hexsplit <- function(x) {
  if(x<0x100) x else c(bitwShiftR(x,8),bitwAnd(x,0xff));
}
