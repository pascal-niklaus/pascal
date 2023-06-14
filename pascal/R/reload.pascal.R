#' Reload package
#'
#' \code{reload.pascal} detach package with unloading
#' 
#' Used only by myself
#' 
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
reload.pascal <- function() {
    detach(package:pascal, unload = TRUE)
    library(pascal)
}
