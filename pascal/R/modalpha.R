#' Modify alpha channel of color
#'
#' Given a vector of colors, modify the alpha channel value by a given
#' factor or change it to a certain value.
#'
#' This function deals with names colors (e.g. 'red') and hexadecimal
#' colors with and without alpha channel (e.g. '#ff000080' or
#' '#ff0000'). If a change factor 'k' is given, the alpha channel
#' value is modified by this factor. If a new 'alpha' value is given,
#' the alpha channel is set to this value.
#'
#' @param x vector of colors (named colors, or in format #RRGGBB or
#'     #RRGGBBAA)
#'
#' @param k factor by which alpha channel value is changed (1.0 = no change).
#'
#' @param alpha new alpha channel value, in range [0,255]
#'
#' @return vector with modified color values
#'
#' @examples
#'
#' colrs <- c('red', '#ff000080', '#0000ff')
#'
#' modalpha(colrs, alpha=0x80)
#' ## [1] "#ff000080" "#ff000080" "#0000ff80"
#' modalpha(colrs, k=0.33)
#' ## [1] "#ff000054" "#ff00002a" "#0000ffNA"
#'
#' @importFrom grDevices col2rgb
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
modalpha <- function(x, k = NULL, alpha = NULL) {
    if (is.null(k) + is.null(alpha) != 1) {
        stop("exactly one of 'k' and 'alpha' needs to be specified.")
    }
    if (!is.null(alpha)) {
        alpha <- min(255, max(0, round(alpha)))
    }
    unname(
        sapply(x, function(x) {
            if (substr(x, 1, 1) != "#") {
                sprintf(
                    "#%s%02x",
                    paste(sprintf("%02x", col2rgb(x)), collapse = ""),
                    if (is.null(k)) alpha else min(255, max(0, round(255 * k)))
                )
            } else {
                sprintf(
                    "%s%02x",
                    substr(x, 1, 7),
                    min(
                        255,
                        max(
                            0,
                            round(
                                if (is.null(k)) {
                                    alpha
                                } else if (nchar(x) == 7) {
                                    255 * k
                                } else {
                                    strtoi(paste("0x", substr(x, 8, 9), sep = "")) * k
                                }
                            )
                        )
                    )
                )
            }
        })
    )
}
