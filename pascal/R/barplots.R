#' Add spacing between bars in bar plot
#'
#' \code{groupSpace} is a helper function to add space between the
#' bars of a bar plot.
#'
#' Given a \code{data.frame} and the names of columns in it, it
#' determines transitions between the groups these columns define and
#' returns 1 for a transition, or 0 otherwise. These values can be
#' passed to the \code{space} argument of \code{barplot}.
#'
#' @param d data frame with data
#'
#' @param cols the names of the columns that define the groups.
#'
#' @param equal logical indicating whether the number of transitions
#'     (when multiple columns are specified) should be returned
#'     instead of a simple vector of \code{1} or \code{0}.
#'
#' @return A vector with values indicating transitions.
#'
#' @examples
#' data(CO2)
#'
#' d <- aggregate(uptake ~ Plant + Type + Treatment, data=CO2, FUN = mean)
#'
#' barplot(d$uptake, names.arg = d$Plant,
#'         space = groupSpace(d, c("Type","Treatment")))
#'
#' barplot(d$uptake, names.arg = d$Plant,
#'         space = groupSpace(d, c("Type","Treatment"), equal = TRUE))
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
groupSpace <- function(d, cols, equal = FALSE) {
    if (is.character(cols))
        cols <- match(cols, names(d))
    pmin(
        if (equal) 1 else +Inf,
        rowSums(apply(
            d[, cols, drop = FALSE],
            2,
            function(x) c(FALSE, as.logical(diff(as.numeric(as.factor(x)))))
        ))
    )
}
