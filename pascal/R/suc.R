#' Unique values, sorted as character
#'
#' This is a convenience function that returns the realised levels of a factor, as character strings, in ascending order.
#'
#' Basically, I often used loops of the form \code{for(a in sort(unique(as.character(some_factor)))) {...}},
#' which I found rather painful to write. This can now be abbreviated as \code{for(a in suc(some_factor)) {...}}.
#' Note that the result is different from \code{levels{some_factor}}, since levels returns all possible levels
#' a factor can take, even if there are no corresponding values in the vector (e.g. due to subsetting).
#' @param x Data vector, typically a factor
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @examples
#' data(CO2, package="datasets")
#' levels(CO2$Plant)
#' ## [1] "Qn1" "Qn2" "Qn3" "Qc1" "Qc3" "Qc2" "Mn3" "Mn2" "Mn1" "Mc2" "Mc3" "Mc1"
#' d <- CO2[1:14,]
#' suc(d$Plant)
#' ## [1] "Qn1" "Qn2"
#' levels(d$Plant)
#' ## [1] "Qn1" "Qn2" "Qn3" "Qc1" "Qc3" "Qc2" "Mn3" "Mn2" "Mn1" "Mc2" "Mc3" "Mc1"
#' @keywords utilities, misc
#' @export
suc <- function(x) 
{ 
  sort(unique(as.character(x))); 
}
