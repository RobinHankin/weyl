`print.weyl` <- function(x,...){
    cat("A member of the Weyl algebra:\n")
    wv <- getOption("weylvars")   # typically c("x","y","z")
    n <- dim(x)
    if(is.null(wv)){ 
        if(n <= 3){
            wv <- letters[seq(from=24,len=n)]
        } else {
            wv <- as.character(seq(from=1,len=n))
        }
    }
    class(x) <- setdiff(class(x),"weyl")
    if(isTRUE(getOption("polyform",default=FALSE))){
        if(n==1){
            options("sprayvars" = c(wv,"d"))
        } else {
            options("sprayvars" = c(paste("x",wv,sep=""),paste("d",wv,sep="")))
        }
        out <- print_spray_polyform(x)
    } else {
        if(n==1){
            options("sprayvars" = c(wv,"d"))
        } else {
            options("sprayvars" = c(paste(" ",wv,sep=""),paste("d",wv,sep="")))
        }
        out <- print_spray_matrixform(x)
    }
    return(weyl(out))
}

