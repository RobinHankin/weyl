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

`rweyl` <- function(n = 9, vals = seq_len(n), arity = 4, powers = 0:2){
    weyl(rspray(n = n, vals = vals, arity = arity, powers = powers))
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
