#' Close all text sinks and graphics devices
#'
#' Close sinks and graphics devices if any are open, issuing a warning.
#' This function is useful when a script fails between opening and closing
#' of a sink, or when these are not balanced.
#' 
#' @return none
#' @examples
#' library(pascal)
#' sink("some_file.txt")
#' pdf("test.pdf")
#' close_all()
#' unlink("test.pdf");
#' unlink("some_file.txt");
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
close_all <- function()
{
  while(!is.null(dev.list())) 
  {
    warning("some devices are open -- closing !!!");
    dev.off();
  }

  while(sink.number() > 0)        
  {
    warning("some sinks are open -- closing !!!");
    sink();
  }
} 

