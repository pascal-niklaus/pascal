safeSetGeneric <- function(name,def) 
{
   if(!isGeneric(name)) {
      setGeneric(name,def)
   } 
}

# handles NULL
recycle <- function(x,i)
{
    if(is.null(x))
        NULL
    else
        x[ 1 + (i-1) %% length(x) ]
}

# works for non-complete recycling
recycledVector <- function(x,length.out)
{
    x[ 1 + 0:(length.out-1) %% length(x) ]
    #rep(x, length.out/length(x))
}
