`weyl` <- function(M){
    if(is.matrix(M)){M <- spray(M)}
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
`as.weyl` <- function(val,d){
    if(is.weyl(val) | is.spray(val)){
        out <- val
    } else if(is.matrix(val)){
        out <- spray(val,d)
    } else if(is.numeric(val)){
        return(val*idweyl(d))
    } else {
        stop("not recognised")
    }
    return(weyl(out))
}

`rweyl` <- function(n = 3, vals = seq_len(n), dim = 3, powers = 0:2){
    weyl(rspray(n = n, vals = vals, arity = dim*2, powers = powers))
}

`coeffs<-` <- function(S,value){UseMethod("coeffs<-")}
`coeffs<-.weyl` <- function(S,value){
    jj <- coeffs(S)
    if(is.disord(value)){
        stopifnot(consistent(coeffs(S),value))
        if((!identical(hash(jj),hash(value))) & (length(value)>1)){stop("length > 1")}
        jj <- value
    } else {
        jj[] <- value  # the meat
    }
    return(weyl(spray(index(S),elements(jj))))
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

`idweyl` <- function(d){weyl(spray(matrix(0,1,d*2),1))}
`as.id.weyl` <- function(S){idweyl(dim(S))}
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

`deg` <- function(S){max(c(-Inf,rowSums(index(S))))} # following Coutinho
`zero` <- function(d){weyl(spray(matrix(0,0,2*d),numeric(0)))}

`as.der` <- function(S){function(x){S*x-x*S}}

