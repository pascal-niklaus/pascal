#' Functions XX to print nice headings and trailers
#'
#' This collection of function is used to print ASCII frames and ``banners''.
#' 
#' @name headingtrailer
#' @rdname headingtrailer
#'
#' @param txt A vector or list of strings that are to be printed in the heading or trailer.
#'            Each element is printed on a separate line
#' @param width Width of the heading or trailer (default: 80 characters)
#' @param right TRUE indicates that the right border of the frame is to be printed (default: FALSE)
#' @param bottom TRUE indicates that the bottom border of the frame is to be printed (default: FALSE)
#' @param char character to draw to horizontal parts of the frame (default is '#')
#' @param side.char character to draw to vertical parts of the frame (default is the same as \code{char})
#' @param corner.char character vector to draw the corners of frames. The elements of the vector are 
#'                    (in this order) top left, top right, bottom left, and bottom right.)
#'                    Default is to use the same character as for horizontal lines.
#' @param spc string used as spacing before and after (left and right) of text lines. 
#'        Usually, these are a series of spaces, but it is possible to use different characters.
#'        Default are two spaces.
#' @param pre Number of empty lines to print before the header/trailer. Default is 0.
#' @param post Number of empty lines to print after the header/trailer. Default is 1.
#' @param center TRUE indicates centered text, FALSE indicates left-aligned text.
#' @return These functions don't return a value
#' @examples
#' heading("Results")
#' ## ############################## 
#' ## # 
#' ## #  Results                 
#' ## # 
#' heading("Results",center=TRUE,bottom=TRUE,right=TRUE,char="-",side.char="|",corner.char=c("/","\\"),width=30)
#' ## /----------------------------\ 
#' ## |                            | 
#' ## |          Results           |
#' ## |                            | 
#' ## \----------------------------/ 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
NULL


#' @rdname headingtrailer
#' @export  
heading <- function(txt=NULL, width=80, right=FALSE, bottom=FALSE, 
                    char='#',spc='  ',side.char=char,corner.char=rep(char,4),
                    post=1,pre=0,center=FALSE)
{
  .heading.internal(txt=txt,width=width,right=right,top=TRUE,bottom=bottom,char=char,spc=spc,side.char=side.char,corner.char=corner.char,pre=pre,post=post,center=center);  
}

#' @rdname headingtrailer
#' @export  
trailer <- function(txt=NULL, width=80, right=FALSE, 
                    char='#',spc='  ',side.char=char,corner.char=rep(char,4),
                    post=1,pre=0,center=FALSE)
{
  .heading.internal(txt=txt,width=width,right=right,top=FALSE,bottom=TRUE,char=char,spc=spc,side.char=side.char,corner.char=corner.char,pre=pre,post=post,center=center);  
}


#' @rdname headingtrailer
#' @export  
simple.heading <- function(txt=NULL, width=80, 
                           char='#',side.char=char,corner.char=rep(char,4),spc='  ',
                           pre=0,post=1,center=TRUE)
{ 
  simple.trailer(txt, width, char, side.char, corner.char, spc, pre, post, center)
}

#' @rdname headingtrailer
#' @export  
simple.trailer <- function(txt=NULL, width=80, 
                           char='#',side.char=char,corner.char=rep(char,4),spc='  ',
                           pre=0,post=1,center=TRUE)
{
    if(pre>0)
      cat(rep("\n",pre));
    tx2 <- paste(spc,as.character(txt),spc,sep='');
    if(center)
      tx2 <- paste(paste(rep(char,floor((width-2*nchar(side.char)-nchar(tx2))/2)-1),collapse=''),tx2,sep='');
    tx2 <- paste(tx2,paste(rep(char,width-nchar(tx2)-2),collapse=''),sep='');    
    cat(corner.char[3],tx2,corner.char[4],"\n",sep='');
    cat(rep("\n",post))
}

.heading.internal <- function(txt, width, right, top, bottom, char,spc,side.char,corner.char,post,pre,center)
{
  if(pre>0) 
    cat(rep("\n",pre))
  if(length(corner.char)==1) 
    corner.char<-rep(corner.char,4)
  else if(length(corner.char)==2)
    corner.char<-c(corner.char,rev(corner.char));
  full1 <- paste(corner.char[1],paste(rep(char,width-2),collapse=''),corner.char[2],sep="");
  full2 <- paste(corner.char[3],paste(rep(char,width-2),collapse=''),corner.char[4],sep="");    
  gap  <- if(right) paste(side.char,paste(rep(' ',width-2*nchar(side.char)),collapse=''),side.char,sep='') else side.char;  
  if(top) 
    cat(full1,"\n");  
  cat(gap,"\n");
  nbody <- width-2*nchar(spc)-2*nchar(side.char);
  for(tx in txt) {       
    tx2 <- as.character(tx);
    if(center)
      tx2 <- paste(paste(rep(' ',floor((nbody-nchar(tx2))/2)),collapse=''),tx2,sep='');
    tx2 <- paste(tx2,paste(rep(' ',nbody-nchar(tx2)),collapse=''),sep='');    
    cat(side.char,spc,tx2,sep="");
    if(right) 
      cat(spc,side.char,sep="");
    cat("\n");
  }
  cat(gap,"\n");
  if(bottom) 
    cat(full2,"\n");
  cat(rep("\n",post));
}
  
