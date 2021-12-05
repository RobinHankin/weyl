`weyl` <- function(M){
    stopifnot(is.ok.weyl(M))
    class(M) <- c("weyl","spray")
    return(M)
}

setOldClass("weyl")

`is.ok.weyl` <- function(M){
    stopifnot(is.spray(M))
    stopifnot(arity(M)%%2 == 0)
    return(TRUE)
}

`is.weyl` <- function(x){inherits(x,"weyl")}

`rweyl` <- function(n = 3, vals = seq_len(n), dim = 3, powers = 0:2){
    weyl(rspray(n = n, vals = vals, arity = dim*2, powers = powers))
}

`coeffs<-.weyl` <- function(S,value){
    class(S) <- setdiff(class(S),"weyl")
    coeffs(S) <- value
    return(weyl(S))
}

`constant.weyl` <- function(x,drop=TRUE){
    class(x) <- setdiff(class(x),"weyl")
    out <- constant(x,drop=TRUE)
    if(drop){
        return(out)
    } else { # not dropped
        return(weyl(spraymaker(spray(matrix(0,1,arity(x)), out),arity=arity(x))))
    }
}

`constant<-.weyl` <- function(x,value){
    class(x) <- setdiff(class(x),"weyl")
    constant(x) <- value
    return(weyl(x))
}

`as.one` <- function(x,d){
    if(is.weyl(x)){
        return(weyl(spray(matrix(0,1,dim(x)*2),1)))
    } else {
        return(weyl(spray(rbind(0,d*2),1)))
    }
}

setGeneric("dim")

`dim.weyl` <- function(x){arity(x)/2}

setGeneric("drop")
setMethod("drop","weyl", function(x){
    if(is.zero(x)){
        return(0)
    } else if(is.constant(x)){
        return(constant(x,drop=TRUE))
    } else {
        return(x)
    }
})
