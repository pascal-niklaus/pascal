#' Stack a data frame
#' 
#' Given a data frame, separate variables (columns) are combined (stacked)
#' into a single column with a new name.
#'
#' \code{stck} and the corresponding \code{splt} were written as a more 
#' generic alternative to \code{\link{merge}}.
#'
#' The \code{factors} and \code{covars} 
#' that are to be preserved in the stacked data frame
#' are passed as character string and can be given new names
#' in the stacked data set. For example, \code{factors=c("f1=factor1","f2=factor2")}
#' will rename the factors \code{factor1} and \code{factor2} to \code{f1} and \code{f2}.
#' If no new name is given, the original is kept.
#' The difference between \code{factors} and \code{covars} is that the \code{covars}
#' are not part of the new combinations generated when \code{expand=TRUE} is specified.
#'
#' The columns that are to be stacked are passed in \code{to.stack} in the
#' form \code{to.stack=c("newcol1=col1,col2,col3","newcol2=col4,col5,col6")}.
#' Several new columns can be generated, but for obvious reasons these 
#' need to be composed of the same number of columns in the original data frame.
#'  
#' \code{cat.names} will assign designators to the groups of data that are stacked. 
#' Let's assume \code{newcol1} is the result of stacking the original 
#' columns (groups) \code{col1}, \code{col2} and \code{col3}. Then, these groups can be iden identified
#' by a new grouping variable defined by \code{cat.names="group=a,b,c"}.
#' All rows with data from \code{col1} will then have \code{group}==\code{"a"}, rows with data from
#' \code{col2} will have \code{group}==\code{"b"} etc. 
#' If no \code{cat.names} are passed, the name of the columns is used instead.
#' @param d Source data frame containing the data set to stack
#' @param factors Character vector containing the names of the factors that
#'        define the categories that are preserved in the new data set
#' @param covars Character vector containing the names of covariables that
#'               are preserved in the new data set
#' @param to.stack Character vector containing the names of the columns that 
#'                are to be stacked in the new data set
#' @param cat.names Character vector containing the names of categories that
#'                correspond to the groups stacked in the new data set
#' @param expand Logical flag: if TRUE, the resulting data frame will contain
#'                 all combinations of the supplied factors, even if these are
#'                 not present in the original data frame
#' @examples
#' data(CO2, package="datasets")
#' d<-CO2
#' d$Replicate<-substr(as.character(d$Plant),3,3)
#' d.split <- splt(d,factors=c("Replicate","Type","conc"),by="Treatment",to.split=c("uptake"))
#' d.split$conc <- safen(d.split$conc)
#' d.split <- d.split[order(d.split$Replicate,d.split$Type,d.split$conc),]
#' d[d$Type=="Mississippi" & d$Replicate==1,]
#' ##    Plant        Type  Treatment conc uptake Replicate
#' ## 43   Mn1 Mississippi nonchilled   95   10.6         1
#' ## 44   Mn1 Mississippi nonchilled  175   19.2         1
#' ## 45   Mn1 Mississippi nonchilled  250   26.2         1
#' ## 46   Mn1 Mississippi nonchilled  350   30.0         1
#' ## 47   Mn1 Mississippi nonchilled  500   30.9         1
#' ## 48   Mn1 Mississippi nonchilled  675   32.4         1
#' ## 49   Mn1 Mississippi nonchilled 1000   35.5         1
#' ## 64   Mc1 Mississippi    chilled   95   10.5         1
#' ## 65   Mc1 Mississippi    chilled  175   14.9         1
#' ## 66   Mc1 Mississippi    chilled  250   18.1         1
#' ## 67   Mc1 Mississippi    chilled  350   18.9         1
#' ## 68   Mc1 Mississippi    chilled  500   19.5         1
#' ## 69   Mc1 Mississippi    chilled  675   22.2         1
#' ## 70   Mc1 Mississippi    chilled 1000   21.9         1
#' d.split[d.split$Type=="Mississippi" & d.split$Replicate==1,]
#' ##   Replicate        Type conc uptake.chilled uptake.nonchilled
#' ## 7         1 Mississippi   95           10.5              10.6
#' ## 2         1 Mississippi  175           14.9              19.2
#' ## 3         1 Mississippi  250           18.1              26.2
#' ## 4         1 Mississippi  350           18.9              30.0
#' ## 5         1 Mississippi  500           19.5              30.9
#' ## 6         1 Mississippi  675           22.2              32.4
#' ## 1         1 Mississippi 1000           21.9              35.5
#' d.stacked <- stck(d.split,
#'                   factors=c("Replicate","Type","conc"),
#'                   to.stack=c("uptake=uptake.chilled,uptake.nonchilled"),
#'                   cat.names=c("Treatment=chilled,nonchilled"))
#' d.stacked$conc <- safen(d.stacked$conc)
#' d.stacked <- d.stacked[order(d.stacked$Replicate,d.stacked$Type,d.stacked$Treatment,d.stacked$conc),]
#' d.stacked[d.stacked$Type=="Mississippi" & d.stacked$Replicate==1,]
#' ##    Replicate        Type conc  Treatment uptake
#' ## 1          1 Mississippi 1000    chilled   21.9
#' ## 2          1 Mississippi  175    chilled   14.9
#' ## 3          1 Mississippi  250    chilled   18.1
#' ## 4          1 Mississippi  350    chilled   18.9
#' ## 5          1 Mississippi  500    chilled   19.5
#' ## 6          1 Mississippi  675    chilled   22.2
#' ## 7          1 Mississippi   95    chilled   10.5
#' ## 43         1 Mississippi 1000 nonchilled   35.5
#' ## 44         1 Mississippi  175 nonchilled   19.2
#' ## 45         1 Mississippi  250 nonchilled   26.2
#' ## 46         1 Mississippi  350 nonchilled   30.0
#' ## 47         1 Mississippi  500 nonchilled   30.9
#' ## 48         1 Mississippi  675 nonchilled   32.4
#' ## 49         1 Mississippi   95 nonchilled   10.6
#' @seealso \code{\link{merge}}, \code{\link{splt}}, \code{\link{expand.grid}}
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @keywords manip datagen utilities misc
#' @export
stck <- function(d, factors=NULL, covars=NULL, to.stack, cat.names=NULL, expand=FALSE)
{
    ## extract factor names 
    facnames    <- sapply(
        factors,
        function(x)
            rev(unlist(strsplit(x,"="),use.names=FALSE))[1]
        )
    facnewnames <- sapply(
        factors,
        function(x)
            unlist(strsplit(x,"="),use.names=FALSE)[1])

    ## extract column indices of factors that will be used... will need that later
    faccols <- match(facnames, names(d))
    levs    <- apply(
        as.matrix(d[,faccols]),
        1,
        function(x)
            paste(x,collapse=":")
        )

    ## if factor combination is not unique to row --> add temporary row number
    rownadded <- FALSE;
    if(length(levs)!=length(unique(levs))) {
        d$..n..stck.. <- 1:nrow(d);
        facnames <- c("..n..stck..",facnames);
        facnewnames < -c("..n..stck..",facnewnames);
        rownadded < -T;
        faccols <- match( facnames, names(d))
        levs    <- apply(
            as.matrix(d[,faccols]),
            1,
            function(x)
                paste(x,collapse=":")
            )
    }

    ## create factor skeleton
    if(expand) {
        faclist <- lapply(
            faccols,
            function(x)
                sort(unique(d[,x]))
            )
        attr(faclist,"names") <- facnewnames;
        dnew <- expand.grid(faclist);
    } else {
        lev <- sort(unique(levs));   # unique
        if(length(faccols)==1)       # need to treat single factor case separately (vector/matrix)
            dnew <- data.frame(lev) 
        else
            dnew <- data.frame(
                t(
                    sapply(
                        lev,
                        function(x)
                            unlist(strsplit(x,":"), use.names=FALSE)
                        )
                    )
                )
        colnames(dnew) <- facnewnames;
    }
    rownames(dnew) <- apply(
        dnew,
        1,
        function(x)
            paste(x,collapse=":")
        )

    ## add covariable columns one by one
    if(!is.null(covars)) {
        covarnames    <- sapply(
            covars,
            function(x)
                rev(unlist(strsplit(x,"="),use.names=FALSE))[1],
            USE.NAMES=FALSE
            )
        covarnewnames <- sapply(
            covars,
            function(x)
                unlist(strsplit(x,"="),use.names=FALSE)[1]
            )

        for(i in 1:length(covarnewnames)) {
            cmd <- paste("dnew$",covarnewnames[i]," <- ",
                "sapply(rownames(dnew),function(x) d$",covarnames[i],"[levs==x], USE.NAMES=FALSE )",sep="");
            eval(parse(text=cmd));
        }
    }

    ## extract all factors
    tostack.names <- sapply(
        to.stack,
        function(x)
            unlist(strsplit(x,"="),use.names=FALSE)[1],
        USE.NAMES=FALSE
        )
    tmp <- sapply(
        to.stack,
        function(x)
            rev(unlist(strsplit(x,"="),use.names=FALSE))[1]
        )
    tostack.vars  <- apply(
        as.array(tmp),
        1,
        function(x)
            unlist(strsplit(x,","),use.names=FALSE)
        )
    colnames(tostack.vars) <- tostack.names

    if(is.null(cat.names)) {
      catcolname <- "group"
      catcolvals <-  tostack.vars[,1]
    } else {
       catcolname <- unlist(strsplit(cat.names,"="),use.names=FALSE)[1]
       if(length(unlist(strsplit(cat.names,"=")))==2) {
           catcolvals <- trim.ws(
               unlist(
                   strsplit(
                       rev(
                           unlist(
                               strsplit(cat.names,"="),
                               use.names=FALSE))[1],
                       ","),
                   use.names=FALSE)
               )
           if(length(catcolvals) != nrow(tostack.vars))
               stop("number of values in cat.names vector does not ",
                    "correspond to number of categories defined by factors");
       } else
           stop("Ill-specified cat.names");
    } 
        
    for(r in 1:nrow(tostack.vars)) { # loop over no of blocks to stack
        cmd <- paste("dnew$",
                     catcolname,
                     " <- as.factor(\"",
                     catcolvals[r],
                     "\")",
                     sep="")
        eval(parse(text=cmd))
        for(v in 1:length(tostack.names)) { # loop over no of vars to stack
            cmd <- paste("dnew$",
                         tostack.names[v],
                         " <- ",
                         "sapply(rownames(dnew),function(x) { d$",
                         tostack.vars[r,v],
                         "[levs==x] } )",
                         sep="")
            eval(parse(text=cmd))
        }
        if(r==1)
            dnewframe <- dnew
        else
            dnewframe <- rbind(dnewframe,dnew);
    }

    rownames(dnewframe) <- 1:nrow(dnewframe)
    if(rownadded)
       dnewframe <- dnewframe[, -which(names(dnewframe)=="..n..stck..")]
    dnewframe
}

