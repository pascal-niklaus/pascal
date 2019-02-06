#' Resistor E12 series values
#'
#' Resistor E12 series values, with R in [1,10[.
#'
#' @docType data
#' @name E12
#'
#' @usage data(E12)
#'
#' @format An vector of class \code{"numeric"}
#'
#' @keywords datasets
#'
#' @examples
#' data(E12)
#' E12
NULL

#' Resistor E24 series values
#'
#' Resistor E24 series values, with R in [1,10[.
#'
#' @docType data
#' @name E24
#'
#' @usage data(E24)
#'
#' @format An vector of class \code{"numeric"}
#'
#' @keywords datasets
#'
#' @examples
#' data(E24)
#' E24
NULL

#' Resistor E48 series values
#'
#' Resistor E48 series values, with R in [1,10[.
#'
#' @docType data
#' @name E48
#'
#' @usage data(E48)
#'
#' @format An vector of class \code{"numeric"}
#'
#' @keywords datasets
#'
#' @examples
#' data(E48)
#' E48
NULL

#' Find closely matching values for resistor divider
#'
#' Find values of 2 or 3 resistors that can be used to get as close a
#' match as possible for a given resister divider ratio. This function
#' is not much optimized in performance or code-wise so far.  When
#' three resistors are used, two may be wired in parallel, and the
#' third in series.
#'
#' @param ratio target ratio of lower resistor to total series resistance.
#' @param rmin minimum total series resistance
#' @param rmax maximum total series resistance
#' @param series resistor series (1 <= values < 10). Defaults to E24 series.
#'        E12 and E48 vectors are available.
#' @param exponents defaults to 0:6, specifying values from 1 Ohm (inclusive) to 10 MOhm (exclusive).
#' @param n number of resistors to use. Defaults to 2, but 3 can also be chosen.
#' @param tol acceptable tolerance for ratio. Defaults to 0.1, which is a maximum of 10\% deviation.
#' @param ntop how many solutions (sorted by deviation) are printed
#' @return data table with results
#' @examples
#' resDivider(.35,rmin=1000,rmax=3000,n=3,tol=.01)
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords utilities misc
#' @importFrom parallel mclapply
#' @export
resDivider <- function(ratio,rmin=1e3,rmax=1e6,series=pascal::E24,exponents=0:6,n=2,tol=.05,ntop=10)
{
    parallel <- (Sys.info()["sysname"]!="Windows") && requireNamespace("parallel")
    myapply <- if(parallel) parallel::mclapply else lapply

    tst <- function(tst) {
        abs(tst/ratio-1) <= tol
    }

    ## create list of available resistors
    fullSeries <- expand.grid(series, 10^(exponents) )
    fullSeries <- fullSeries[,1] * fullSeries[,2]

    ## try 2-resistor combinations
    res <-
        myapply(fullSeries [ ( fullSeries >= rmin) & (fullSeries <= rmax) ],
                function(A) {
                    Bvec <- fullSeries[ (fullSeries >= A) & (fullSeries <= rmax-A) ]
                    if(length(Bvec)>0) {
                        res <- data.frame(A=rep(NA,length(Bvec)*2), B=NA, C=NA,
                                          r=NA, total=NA,
                                          mode="", stringsAsFactors = FALSE)
                        i <- 1
                        for(B in Bvec) {
                            if( ( A + B <= rmax) && (A + B >= rmin) ) {
                                if(tst(A/(A+B))) {
                                    res$A[i] <- A; res$B[i] <- B
                                    res$r[i] <- A/(A+B); res$total[i] <- A+B
                                    res$mode[i] = "B--A"
                                    i <- i+1
                                }
                                if(tst(B/(A+B))) {
                                    res$A[i] <- A; res$B[i] <- B
                                    res$r[i] <- B/(A+B); res$total[i] <- A+B
                                    res$mode[i] = "A--B"
                                    i <- i+1
                                }
                            }
                        }
                        res[is.finite(res$A),]
                    } else
                        NULL

                })
    res <- do.call("rbind",res)


    ## 3-resistor combinations
    if(n==3) {
        res.p <- myapply(
            fullSeries [ fullSeries <= rmax ],
            function(C) {
                imax <- 0
                istep <- 1000
                res <- data.frame(stringsAsFactors = FALSE)
                i <- 1
                for(A in fullSeries) {
                    for(B in fullSeries) {
                        if(i >= imax) {
                            res <- rbind(res,
                                         data.frame(A=rep(NA,istep),
                                                    B=NA,
                                                    C=NA,
                                                    r=NA,
                                                    total=NA,
                                                    mode="",
                                                    stringsAsFactors = FALSE))
                            imax <- imax + istep
                        }
                        AB <- 1 / ( 1/A + 1/B )
                        if(A+B+C >= rmin && A+B+C <= rmax) {
                            tmp <- (B+C) / (A+B+C)
                            if(tst(tmp)) {
                                res$A[i] <- A; res$B[i] <- B; res$C[i] <- C
                                res$r[i] <- tmp; res$total[i] <- A+B+C
                                res$mode[i] <- "A--(B+C)"
                                i <- i+1
                            }
                            tmp <- (C) / (A+B+C)
                            if(tst(tmp)) {
                                res$A[i] <- A; res$B[i] <- B; res$C[i] <- C
                                res$r[i] <- tmp; res$total[i] <- A+B+C
                                res$mode[i] <- "(A+B)--C"
                                i <- i+1
                           }
                        }
                        if(AB+C >= rmin && AB+C <= rmax) {
                            tmp <- (C) / (AB+C);
                            if(tst(tmp)) {
                                res$A[i] <- A; res$B[i] <- B; res$C[i] <- C
                                res$r[i] <- tmp; res$total[i] <- AB+C
                                res$mode[i] <- "(A||B)--C"
                                i <- i+1
                            }
                            tmp <- (AB) / (AB+C)
                            if(tst(tmp)) {
                                res$A[i] <- A; res$B[i] <- B; res$C[i] <- C
                                res$r[i] <- tmp; res$total[i] <- AB+C
                                res$mode[i] <- "C--(A||B)"
                                i <- i+1
                            }
                        }
                    }
                }
                res[ is.finite(res$A), ]
            }
        )
        res.p <- do.call("rbind",res.p)

        ## eliminate duplicates
        idx <- rep(TRUE,nrow(res.p)) # true = keep
        to.check <- idx

        bc.ser <- grepl("(B+C)",res.p$mode,fixed=TRUE)
        ab.par <- grepl("(A||B)",res.p$mode,fixed=TRUE)
        ab.ser <- grepl("(A+B)",res.p$mode,fixed=TRUE)

        if(nrow(res.p)>1)
            for(i in 1:(nrow(res.p)-1)) {
                if(idx[i]) {
                    to.check[i] <- FALSE

                    ra <- res.p$A[i]
                    rb <- res.p$B[i]
                    rc <- res.p$C[i]

                    if(ab.ser[i])
                        idx[ to.check & ab.ser & ( (ra == res.p$A & rb == res.p$B) | (ra == res.p$B & rb == res.p$A) ) ] <- FALSE
                    if(ab.par[i])
                        idx[ to.check & ab.par & ( (ra == res.p$A & rb == res.p$B) | (ra == res.p$B & rb == res.p$A) ) ] <- FALSE
                    if(bc.ser[i])
                        idx[ to.check & bc.ser & ( (rc == res.p$C & rb == res.p$B) | (rc == res.p$B & rb == res.p$C) ) ] <- FALSE
                }

            }
        res.p <- res.p[idx,]
        res <- rbind(res, res.p)
    } # if(n==3)

    res$dev <- 100 * (res$r / ratio - 1);
    res<-res[(order(abs(res$dev))),]
    res$dev<-sprintf("%5.2f%%",res$dev);
    rownames(res)<-NULL;

    cat( "****************** Best solutions with 2 resistors *****************\n");
    tmp<-res[is.na(res$C),];
    if(nrow(tmp)>ntop)
        tmp<-tmp[1:ntop,]
    rownames(tmp)<-NULL;
    if(nrow(tmp)==0)
        cat("-- none --\n")
    else
        print(tmp[,!grepl("^C$",names(tmp))])

    if(n==3) {
        cat("\n***************** Best solutions with 3 resistors *****************\n");
        tmp<-res[is.finite(res$C),];
        if(nrow(tmp)>ntop)
            tmp<-tmp[1:ntop,]
        rownames(tmp)<-NULL;
        if(nrow(tmp)==0)
            cat("-- none --\n")
        else
            print(tmp)
    }
    invisible(res);
}
