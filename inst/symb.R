`symb` <- function(W){
    stopifnot(dim(W)==1)
    ind <- index(W)
    val <- elements(coeffs(W))
    out <- ""

    dn <- function(n){ # differentiate f(x) 'n' times
        if(n==0){
            jj <- "f(x)"
        } else if (n==1){
            jj <- "f'(x)"
        } else {
        paste("f^{(",n,")}",collapse="","(x)",sep="")
        }
    }
    pn <- function(n){
        if(n==0){""}else if(n==1){"*x"}else{paste("*x^",n,sep="")}
    }

    for(i in seq_len(nrow(ind))){
        jj <- val[i]
        if(val[i]>0){jj <- paste("+",jj,sep="")}
        out <- paste(out,jj,pn(ind[i,1]),"*",dn(ind[i,2]),sep="")
    }
    return(out)
}
    
