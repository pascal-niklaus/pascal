#' Generate path diagram using graphViz
#'
#' Deprecated -- is just there for compatibility with old code
#' and will eventually be removed
#' @param sem.fit sem object
#' @param pdfFile file name of outout
#' @param scaling.factor xxx
#' @param show.values xxx
#' @param boxtext xxx
#' @param dispBIC xxx
#' @return NULL
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' ##  @export
## niceDiag <- function(sem.fit,pdfFile="fit.pdf",scaling.factor=1,show.values=TRUE,boxtext=NULL,dispBIC=FALSE)
## {
##   uniq.name <- function(x) {
##     pattern <- "^([^\\<\\-]+) [\\<\\>\\-]+ (.*)$";
##     sapply(as.character(x),
##            function(x) {
##                           paste(sort(c(sub(pattern,"\\1",x),sub(pattern,"\\2",x))),collapse="/");
##                        }
##           );
##   }
  
##   pathDiagram(sem.fit,"tmp",ignore.double=F);
##   dot.lines <- readLines("tmp.dot");
##   fit<-stdCoef(sem.fit);
##   fit$uname<-uniq.name(fit[,3])
##   sig<-summary(sem.fit)$coef;
##   sig$uname<-uniq.name(sig[,5])
  
##   if(dispBIC)
##       boxtext<-paste("BIC",sprintf("%.3f",summary(sem.fit)$BIC));

##   pattern2 <- "^ *\"([^\"]+)\" *[\\<\\>\\-]+ *\"([^\"]+)\".*$";

##   for(i in 1:length(dot.lines)) {
##     if(grepl(pattern2,dot.lines[i])) {
 
##       uname <- paste(sort(c(sub(pattern2,"\\1",dot.lines[i]),sub(pattern2,"\\2",dot.lines[i]))),collapse="/");
##       val   <- fit[match(uname,fit$uname),2]      
##       P     <- sig[match(uname,sig$uname),4]      
##       Ptxt <- if(is.na(P)) "NA" else if(P<0.001) "***" else if(P<0.01) "**" else if(P<0.05) "*" else if(P<0.1) "(*)" else "n.s.";
##       pen.width <- scaling.factor*val;                    
##       txt <- paste(sprintf("%.2f",val),Ptxt);
##       dot.lines[i]<-gsub("label=\"[^\"]*\"","",dot.lines[i]);
##       if(show.values) {          
##         dot.lines[i]<-gsub("\\[",sprintf("[ label=\"%s\" ",txt),dot.lines[i])
##       } 

##       dot.lines[i]<-gsub(
##                 "];",
##                 paste(" penwidth=",round(abs(pen.width),3),
##                       " color=\"#",ifelse(pen.width>0,"b00000","0000b0"),"\"","];",sep=""),                    
##                 dot.lines[i]);                
##     }
##   }
  
##   if(!is.null(boxtext))
##     dot.lines <- append(dot.lines, paste(" \"BOX\" [label=\"",boxtext,"\",color=yellow,style=filled];",sep=""), length(dot.lines)-1)
##   writeLines(dot.lines,"tmp.processed.dot");
##   system(paste("dot -Tpdf -o \"",pdfFile,"\" tmp.processed.dot",sep=""));
##   unlink("tmp.dot");
##   unlink("tmp.processed.dot");
## }

