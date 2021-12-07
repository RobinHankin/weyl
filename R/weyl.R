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

`is.weyl` <- function(M){inherits(M,"weyl")}

`rweyl` <- function(n = 3, vals = seq_len(n), dim = 3, powers = 0:2){
    weyl(rspray(n = n, vals = vals, arity = dim*2, powers = powers))
}

`coeffs<-` <- function(S,value){UseMethod("coeffs<-")}
`coeffs<-.weyl` <- function(S,value){
    class(S) <- setdiff(class(S),"weyl")
    coeffs(S) <- value
    return(weyl(S))
}

`constant` <- function(x,drop=TRUE){UseMethod("constant")}
`constant.weyl` <- function(x,drop=TRUE){
    class(x) <- setdiff(class(x),"weyl")
    out <- constant(x,drop=TRUE)
    if(drop){
        return(out)
    } else { # not dropped
        return(weyl(spraymaker(spray(matrix(0,1,arity(x)), out),arity=arity(x))))
    }
}

`constant<-` <- function(x, value){UseMethod("constant<-")}
`constant<-.weyl` <- function(x,value){
    class(x) <- setdiff(class(x),"weyl")
    constant(x) <- value
    return(weyl(x))
}

`id` <- function(d){weyl(spray(matrix(0,1,d*2),1))}
`as.id` <- function(S){id(dim(S))}
`is.id` <- function(S){S == as.id(S)}

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

`deg` <- function(S){max(rowSums(index(S)))} # following Coutinho
`zero` <- function(d){weyl(spray(matrix(0,0,2*d),numeric(0)))}
