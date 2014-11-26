#' Get current screen resolution
#'
#' This function calls \code{xrandr} to get the resolution of the display in use.
#' If the system is not linux (and xrandr not available), it returns a width of 1280 and a height of 1024 pixels.
#' This could be fixed by adding platform-specific code.
#' 
#' @return A list with elements \code{width} and \code{height}
#' @examples
#' getResolution()
#' ## $width
#' ## [1] 1366
#' ##
#' ## $height
#' ## [1] 768
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
getResolution <- function()
{
  if(tolower(Sys.info()["sysname"])=="linux") {
    x <- system("xrandr | grep current",intern=TRUE)
    width<- safen(gsub(".*current (\\d+) x (\\d+).*","\\1",x));
    height<- safen(gsub(".*current (\\d+) x (\\d+).*","\\2",x));
  } else {
    width=1280;
    height=1024;
  }
  list(width=width,height=height);
}
