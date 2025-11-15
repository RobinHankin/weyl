#' @export
`[.weyl` <- function(x,...){
    coeffs(x)[!(...)] <- 0
    return(x)
}

#' @export
`[<-.weyl` <- function(x,index,value){
    coeffs(x)[index] <- value
    return(x)
}
