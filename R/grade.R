`grades` <- function(x){  #special dispensation for the zero clifford object
    if(is.zero(x)){
        out <- numeric(0)
    } else {
        out <- disord(rowSums(index(x)),hashcal(x))
    }
    return(out)
}

`grade` <- function(C,n,drop=TRUE){
    g <- elements(grades(C))
    wanted <- g %in% n
    if(any(wanted)){
        out <- weyl(spray(index(C)[wanted,],elements(coeffs(C))[wanted]))
    } else {
        out <- weyl(spray(matrix(0,0,2*dim(C)),0))
    }
    if(drop){out <- drop(out)}
    return(out)
}

`grade<-` <- function(C,n,value){
    coeffs(C)[grades(C) %in% n] <- value
    return(C)
}
