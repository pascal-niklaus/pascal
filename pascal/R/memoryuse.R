#' Display memory use statistcs
#'
#' Displays (or returns) total memory use after garbage collection and
#' the largest R objects present in an environment (by default: the
#' global environment).  This function may be useful to identify
#' memory hogs.
#'
#' Sometimes it is useful to get a quick overview of the largest
#' objects in a particular environment.  \code{memused} does exactly
#' this. By default, the largest objects in the global environment are
#' shown. If run within a function, one may wish to show objects in
#' the local environment instead.  See the examples below to learn
#' more about its use.
#'
#' @param print logical, indicating whether results should be printed
#'     (\code{TRUE}, default) or returned (\code{FALSE})
#'
#' @param total logical, indicating whether the total memory used
#'     should be reported. This is the data returned by a call to gc()
#'
#' @param objs logical, indicating whether the memory use of the
#'     \code{top} largest objects in environment \code{env} should be
#'     listed.
#'
#' @param top number of objects that should be included in the
#'     list. The remaining are listed as totals at the end of the
#'     table.
#'
#' @param base2 logical, indicating whether memory units are according
#'     to SI (base 1000) or base 2. Note that, for example,
#'     \code{gc()} reports 1MB=2^20=104856 bytes (default
#'     \code{FALSE}).
#' #'
#' @param env The environment from which objects should be
#'     reported. Defaults to the global environment.
#'
#' @return numeric value with total memory used as reported by gc(),
#'     in bytes (\code{total=TRUE}), a \code{data.frame} with object
#'     memory use (\code{objs=TRUE}), or a list of both.
#'
#' @seealso \code{\link{gc}}
#' @examples
#' a <- matrix(rnorm(1e6),1000,1000)
#' b <- LETTERS[1:26]
#' c <- integer(100000)
#' memused()
#' ## -----------------  memory use  -------------------
#' ## Total:  135.4 MByte
#' ## Largest objects in R_GlobalEnv:
#' ##              class      type   bytes percent       hsize
#' ## a     matrix,array    double 8000216  95.22%   8.0 MByte
#' ## c          integer   integer  400048   4.76% 400.0 KByte
#' ## b        character character    1712   0.02%  1712 Bytes
#' ## TOTAL                  TOTAL 8401976 100.00%   8.4 MByte
#'
#' fun <- function(x) { a<-3; memused(env=environment()) }
#' fun(1:100)
#' ## -----------------  memory use  -------------------
#' ## Total:  135.4 MByte
#' ## Largest objects in :
#' ##         class    type bytes percent       hsize
#' ## x     integer integer   448  88.89%   448 Bytes
#' ## a     numeric  double    56  11.11%    56 Bytes
#' ## TOTAL           TOTAL   504 100.00%   504 Bytes
#'
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#'
#' @importFrom utils object.size
#'
#' @export
memused <- function(print=TRUE, total=TRUE, objs=TRUE, top=10, base2=FALSE, env=globalenv()) {
    m <- if(total) {
             tmp <- as.data.frame(gc())
             sz <- round(1024*1024*tmp[,6]/tmp[,5])
             sum(tmp[,1]*sz)
         } else NA

    r <- NA
    if (objs) {
        r <- sapply(ls(envir=env),
                    function(x) object.size(get(x=x, envir=env)))
        if (length(r)>0) {
            r <- data.frame(row.names=names(r),
                            class=sapply(names(r),
                                         function(x) paste(class(get(x=x, envir=env)),collapse=",")),
                            type=sapply(names(r),
                                        function(x) typeof(get(x=x, envir=env))[1]),
                            bytes=r)
            r <- r[order(r$bytes, decreasing = TRUE),,drop=FALSE]

            if (nrow(r)>top) {
                r$class[top] <- sprintf("%d objects",nrow(r)-top+1)
                r$type[top] <- ""
                rownames(r)[top]<-"..."
                r$bytes[top] <- sum(r$bytes[top:nrow(r)])
                r<- r[1:top,]
            }

            r$percent <- sprintf("%.2f%%",100*r$bytes/sum(r$bytes))

            r <- rbind(r, data.frame(
                              row.names = "TOTAL",
                              class="",
                              type="TOTAL",
                              bytes=sum(r$bytes),
                              percent="100.00%"))
            r$hsize <- byteunits(r$bytes, base2=base2)
        } else
            r <- NULL
    }

    if(print) {
        simple.heading("memory use", char="-", width=60, post=0)
        if(total)
            cat("Total: ",trimws(byteunits(m, base2)),"\n")
        if(objs) {
            cat("Largest objects in ",environmentName(env),":\n", sep="")
            base::print(r)
        }
    } else {
        if(total && objs)
            return(list(total=m, objs=r))
        if(total)
            return(m)
        if(objs)
            return(r)
    }
}

byteunits <- function(x, base2=FALSE) {
    pfx <- if(base2)
               c("Bytes   ","KibiByte","MebiByte","GibiByte","TebiByte","PebiByte","Exbibyte")
           else
               c("Bytes","KByte","MByte","GByte","TByte","PByte","EByte")
    k <- if(base2) 1024 else 1000
    sapply(x,
           function(x) {
               d <- 1
               if(x < 20*k)
                   return(sprintf("%5d %s", x, pfx[1]))
               for(i in 2:length(pfx)) {
                   x <- x / k
                   if (x < 999*d)
                       return(sprintf("%5.1f %s",x,pfx[i]))
               }
           })
}
