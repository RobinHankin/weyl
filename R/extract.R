
`[.weyl` <- function(x,...){
    coeffs(x)[!(...)] <- 0
    return(x)
}

`[<-.weyl` <- function(x,index,value){
    coeffs(x)[index] <- value
    return(x)
}
