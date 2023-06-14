#' Power transformation for positive and negative values
#'
#' Joins two power or log transformations (one for positive and
#' one for negative values). The two functions are joined so that they
#' have a common slope at x=0. This is achieved by applying an x-axis offset.
#'
#' \code{power.posexp} was written to provide a continuous and differentiable
#' data transformation function that differs for positive and negative values.
#' The data transformation applied has the form
#'
#' \code{y = sign(y) * abs(x-xoffset)^xexp + yoffset}, 
#'
#' where \code{xexp} is specified (\code{posexp} or \code{negexp}).
#' \code{xoffset} is then chosen so that the slope at x=0 equals
#' the one specified in the function call (\code{slope}).
#' yoffset if chosen so that y=0 for x=0.
#' In case of \code{posexp} or \code{negexp = "log"},
#' the function applied is 
#'
#' \code{y = sign(y) * log(x-xoffset) + yoffset},
#'
#' with xoffset and yoffset chosen similarly.
#'
#' \code{power.posexp.backtransform} calculates the inverse of \code{power.posneg}. 
#' @param x {Data to be transformed}
#' @param posexp Exponent for positive x, or \code{"log"} for log transformation.
#' @param negexp Exponent for negative x, or \code{"log"} for log transformation.
#'        Note that the function is applied to abs(x), and the sign of 
#'        the result inverted.
#' @param slope The slope with which the positive and negative transformation
#'              functions join.
#' @examples
#' x <- seq(-100,100,length=100)
#' y<-power.posneg(x,posexp=1/2,negexp=2/3,slope=1)
#' plot(x,y,ty="l")
#' x.back<-power.posneg.backtransform(y,posexp=1/2,negexp=2/3,slope=1)
#' range(x.back-x)
#' @note This can be a strange transformation. You need to know what you are doing.
#' @seealso \code{\link{mean}} \code{\link{var}} \code{\link{se}} \code{\link{sd}} \code{\link{median}} \code{\link{min}} \code{\link{max}}  
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @rdname pwrposneg
#' @export  
power.posneg <- function(x, posexp = .5,negexp = .5,slope = 1) {
    if (posexp == "log") {
        posalpha <- 1 / slope
        posyoffset <- log(posalpha)
    } else {
        posalpha <- (slope / posexp)^(1 / (posexp - 1))
        posyoffset <- posalpha^posexp
    }

    if (negexp == "log") {
        negalpha <- 1 / slope
        negyoffset <- log(negalpha)
    } else {
        negalpha <- (slope / negexp)^(1 / (negexp - 1))
        negyoffset <- negalpha^negexp
    }

    sapply(
        x,
        function(x) {
            if (is.na(x)) {
                NA
            } else if (x < 0) {
                if (negexp == "log") {
                    -(log(-x + negalpha) - negyoffset)
                } else {
                    -((-x + negalpha)^negexp - negyoffset)
                }
            } else {
                if (posexp == "log") {
                    log(x + posalpha) - posyoffset
                } else {
                    ((x + posalpha)^posexp - posyoffset)
                }
            }
        }
    )
}

#' @rdname pwrposneg
#' @export
power.posneg.backtransform <- function(x, posexp = .5, negexp = .5, slope = 1) {
    if (posexp == "log") {
        posalpha <- 1 / slope
        posyoffset <- log(posalpha)
    } else {
        posalpha <- (slope / posexp)^(1 / (posexp - 1))
        posyoffset <- posalpha^posexp
    }

    if (negexp == "log") {
        negalpha <- 1 / slope
        negyoffset <- log(negalpha)
    } else {
        negalpha <- (slope / negexp)^(1 / (negexp - 1))
        negyoffset <- negalpha^negexp
    }

    sapply(
        x,
        function(x) {
            if (is.na(x)) {
                NA
            } else if (x < 0) {
                if (negexp == "log") {
                    negalpha - exp(negyoffset - x)
                } else {
                    negalpha - (negyoffset - x)^(1 / negexp)
                }
            } else {
                if (posexp == "log") {
                    exp(posyoffset + x) - posalpha
                } else {
                    (x + posyoffset)^(1 / posexp) - posalpha
                }
            }
        }
    )
}
