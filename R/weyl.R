`weyl` <- function(M){
    stopifnot(is.ok.weyl(M))
    class(M) <- c("weyl","spray")
    return(M)
}

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

`constant.weyl` <- function(x,drop=FALSE){
    class(x) <- setdiff(class(x),"weyl")
    out <- constant(x,drop=FALSE)
    if(drop){
        return(drop(out))
    } else {
        return(weyl(out))
    }
}

`constant<-.weyl` <- function(x,value){
    class(x) <- setdiff(class(x),"weyl")
    constant(x) <- value
    return(weyl(x))
}

`as.one` <- function(x,dim){
    if(is.weyl(x)){
        return(spray(index(x)*0,1))
    } else {
        return(weyl(spray(rbind(0,dim*2)),1))
    }
}



setGeneric("dim")

`dim.weyl` <- function(x){ arity(x)/2}

