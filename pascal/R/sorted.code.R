#' Create unique ID from separate parts that are sorted
#' 
#' \code{sorted.code} constructs IDs from individual parts, making
#' sure that these parts sorted.code(c("a:b:c","z:x:y","b:a"),split=":",collapse="-") # case 3 are sorted in alphabetical order before being
#' mergheses/Adele Ferrari/ed. This can be used e.g. to create unambiguous label for
#' community compositions, given sets of species.
#' 
#' The parts to be merged can be passed in four different forms:
#' \enumerate{
#'    \item as elements in separate (parallel) vectors.
#'    \item as a matrix with parts to be combined in rows.
#'    \item as string, separated by a character specified in \code{split}.
#'    \item as list of vectors. 
#' }
#' The two last forms are useful if variable numbers of parts are to be combined.
#' 
#' @param ... data to be combined, in one of the forms outlined above
#' @param split character separating parts, if parts already combined in strings
#' @param collapse character used to merge parts after sorting
#' @return vector of strings with merged parts
#'
#' @examples
#' v1 <- sample(LETTERS,10)
#' v2 <- sample(LETTERS,10)
#' m <- cbind(v1,v2)
#' sorted.code(v1,v2,collapse="-")   # case 1
#' sorted.code(m,collapse="-")       # case 2
#' sorted.code(c("a:b:c","z:x:y","b:a"),split=":",collapse="-") # case 3 
#' sorted.code(list(c("a","b","c"),c("z","y")),collapse="-")    # case 4
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export
sorted.code <- function(...,split=NULL,collapse=NULL)
{
    p <- list(...)
    if(is.list(p[[1]])) {
        p <- p[[1]]      
    } else {
        if(is.matrix(p[[1]]))
            p <- p[[1]]
        else
            p <- matrix(unlist(p),nrow=length(p[[1]]))
        if(!is.null(split))
            if(ncol(p)>1)
                stop("split specified call in multiple column form")
            else
                p <- strsplit(p,split,fixed=TRUE)
    }
    if(is.null(collapse))
        if(is.null(split))
            stop("collapse not specified")
        else
            collapse <- split
    if(is.list(p))
        sapply(p,function(x) paste(sort(x),collapse=collapse) )
    else
        apply(p,1,function(x) paste(sort(x),collapse=collapse) )
}


