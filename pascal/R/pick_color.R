#' Pick named colors
#'
#' \code{pick_color} shows named colors and allows to select these by
#' left-clicking on them with the mouse. Right-clicking terminates the
#' color selection.
#'
#' It is possible to only show colors in a certain hue range. This is
#' done by passing a \code{hue} argument, which is either numeric
#' (range 0 to 1) or a text that is searched within the known color
#' names. The hue value is then calculated as the average hue of the
#' names that match the search.  Then, all colors within a hue range
#' of [-0.1, 0.1] around that mean value are presented.
#'
#' Because there are 100 named grey values, only the ones in intervals
#' of 10 are shown (e.g. gray10, gray20 to gray90). Also, by default,
#' only colors with a minimum saturation of 0.2 are shown. This can be
#' changed with the \code{minsat} argument.
#'
#' Upon exit, a code fragment containing a vector with the chosen
#' color names is placed in the clipboard. It can then be pasted into
#' code.
#'
#' @param minsat minimum saturation of colors that are to be shown
#'
#' @param hue hue of the color range to be shown. This may be a word
#'     (e.g. "blue") or a numeric value in the range [0, 1[.
#'
#' @return Data frame with color properties
#'
#' @seealso \code{\link{colors}}
#' @importFrom grDevices colors rgb2hsv
#'
#' @examples
#' \dontrun{
#' pick_color()
#' pick_color("olive")
#' }
#' @export
pick_color <- function(hue = NULL, minsat = 0.2) {
    tmp <- data.frame(name = colors())
    tmp$hex <- pascal::modalpha(tmp$name, k = 1)
    tmp$r <- pascal::safen(paste0("0x", substr(tmp$hex, 2, 3)))
    tmp$g <- pascal::safen(paste0("0x", substr(tmp$hex, 4, 5)))
    tmp$b <- pascal::safen(paste0("0x", substr(tmp$hex, 6, 7)))
    tt <- rgb2hsv(col2rgb(colors()))
    tmp$h <- tt[1, ]
    tmp$s <- tt[2, ]
    tmp$v <- tt[3, ]
    rm(tt)

    ## remove "gray"s and keep only a few "grey"s
    tmp <- tmp[!grepl("^gray", tmp$name, perl = TRUE), ]
    tmp <- tmp[tmp$s > 0 | grepl("^grey[1-9]0", tmp$name, perl = TRUE), ]

    ## remove colors that run under two different names
    tmp <- tmp[!duplicated(tmp[, c("r", "g", "b")]), ]

    ## remove colors with too low saturation
    tmp <- tmp[(tmp$s == 0) | (tmp$s >= minsat), ]

    ## restrict hue if necessary
    if (!is.null(hue)) {
        tmp <- tmp[tmp$s > 0, ]
        if (is.character(hue)) {
            idx <- grep(hue, tmp$name, fixed = TRUE)
            hue <- colSums(pascal::pol2cart(1, tmp$h[idx] * 2 * pi))
            hue <- pascal::cart2pol(hue)$phi / (2 * pi)
        }
        hue <- (hue + c(-.1, .1)) %% 1
        if (hue[1] < hue[2]) {
            tmp <- tmp[(tmp$h > hue[1]) & (tmp$h < hue[2]), ]
        } else {
            tmp <- tmp[(tmp$h > hue[1]) | (tmp$h < hue[2]), ]
        }
    }

    w <- round(sqrt(nrow(tmp) * 1.2))

    tmp$y <- floor((seq_len(nrow(tmp)) - 1) / w)
    tmp$x <- (seq_len(nrow(tmp)) - 1) %% w

    par(mar = c(0.1, 0.1, 0.1, 0.1))
    plot(NULL,
        xlim = range(tmp$x) + c(0, 1), ylim = range(tmp$y) + c(0, 1),
        xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
        xlab = "", ylab = "", bty = "n"
    )

    rect(tmp$x, tmp$y, tmp$x + 1, tmp$y + 1, col = tmp$name, border = "black")
    text(tmp$x + 0.5, tmp$y + 0.5, tmp$name,
        cex = 1, srt = 20, col = pascal::modalpha("black", k = 0.6)
    )

    r <- graphics::identify(
        x = tmp$x + 0.5, y = tmp$y + 0.5, labels = tmp$name,
        plot = FALSE, tolerance = 1, order = TRUE
    )
    r <- r$ind[r$order]
    tmp <- tmp[r, ]
    tmp$x <- NULL
    tmp$y <- NULL
    row.names(tmp) <- NULL

    if (requireNamespace("clipr")) {
        clipr::write_clip(
            paste0(
                "c(",
                paste(sprintf("\"%s\"", tmp$name), collapse = ", "),
                ")"
            )
        )
    }
    dev.off()
    tmp
}
