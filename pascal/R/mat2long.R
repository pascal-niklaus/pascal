#' Convert between matrix and long (sparse) format
#'
#' Interconvert data between matrix and long format.
#'
#' \code{mat2long} converts data in matrix format into a
#' \code{data.frame} with columns containing the row names, column
#' names, and the actual data. \code{long2mat} does the reverse. For
#' both functions, it is possible to apply a predicate to include only
#' values that satisfy that filter -- all other values are set to
#' \code{NA}.
#'
#' @param m matrix
#'
#' @param row.names,col.names optional vectors with row and column
#'     names. If not provided, row and column names are taken from the
#'     matrix \code{m}.
#'
#' @param names names of row, column, and data columns in the
#'     resulting \code{data.frame}. Defaults to \code{"row", "col",
#'     "data"}.
#'
#' @param filter predicate indicating which elements to include
#'     (default = all). For example, one could exclude non-finite
#'     values (\code{NA, NaN, Inf}) by specifying
#'     \code{filter=is.finite}.
#'
#' @return Data frame containing long format.
#'
#' @examples
#' ## distances between US states
#' m <- as.matrix(dist(cbind(state.center$x, state.center$y)))
#' dimnames(m) <- list(state.abb, state.abb)
#' m[!lower.tri(m)] <- NA
#' m <- m[1:10, 1:10]
#' d <- mat2long(m, filter = is.finite)
#' d
#'
#' long2mat(d)
#' long2mat(d, filter = function(x) x > 30)
#' @rdname mat2long
#' @export
mat2long <- function(m, names = c("row", "col", "data"),
                     row.names = rownames(m), col.names = colnames(m),
                     filter = NULL) {
    if (is.null(row.names)) {
        row.names <- seq_len(nrow(m))
    } else if (length(row.names) != nrow(m)) {
        stop("Dimension of matrix and length of row.names conflict!")
    }
    if (is.null(col.names)) {
        col.names <- seq_len(ncol(m))
    } else if (length(col.names) != ncol(m)) {
        stop("Dimension of matrix and length of col.names conflict!")
    }

    res <- data.frame(
        row = row.names,
        col = rep(col.names, each = nrow(m)),
        value = as.vector(m)
    )
    if (!is.null(filter)) {
        res <- res[filter(res$value), ]
    }
    names(res) <- names
    row.names(res) <- NULL
    res
}

#' @param d,cols data in long format (typically a
#'     \code{data.frame}). \code{cols} indicates which columns contain
#'     row, column and data.
#' @rdname mat2long
#' @export
long2mat <- function(d, cols = 1:3,
                     row.names = NULL, col.names = NULL,
                     filter = NULL) {
    sep <- "\U1f702"
    if (is.character(cols)) cols <- match(cols, names(d))
    if (is.null(row.names)) row.names <- pascal::suc(d[, cols[1]])
    if (is.null(col.names)) col.names <- pascal::suc(d[, cols[2]])
    nr <- length(row.names)
    nc <- length(col.names)
    ir <- match(d[, cols[1]], row.names)
    ic <- match(d[, cols[2]], col.names)
    if (any(is.na(ir))) {
        missings <- pascal::suc(d[is.na(ir), cols[1]])
        stop(
            "data contains row names that are not in 'row.names': ",
            paste(missings, collapse = ", ")
        )
    }
    if (any(is.na(ic))) {
        missings <- pascal::suc(d[is.na(ic), cols[2]])
        stop(
            "data contains column names that are not in 'col.names': ",
            paste(missings, collapse = ", ")
        )
    }

    idx <- match(
        paste(rep(row.names, each = nc), col.names, sep = sep),
        paste(d[, cols[1]], d[, cols[2]], sep = sep)
    )

    m <- matrix(d[idx, cols[3]], nrow = nr, ncol = nc, byrow = TRUE)
    dimnames(m) <- list(row.names, col.names)

    ## set filtered-out results to NA
    if (!is.null(filter)) {
        m[! apply(m, 1:2, filter)] <- NA
    }
    m
}
