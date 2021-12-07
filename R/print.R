`print.weyl` <- function(x,...){
    cat("A member of the Weyl algebra:\n")
    jj <- getOption("weylvars")   # typically c("x","y","z")
    if(!is.null(jj)){ 
        options("sprayvars" = c(jj,paste("d",jj,sep="")))
    } else {  # weylvars is NULL -> unset
        n <- arity(x)/2
        if(n <= 3){
            jj <- letters[seq(from=24,len=n)]        
            jj <- c(jj,paste("d",jj,sep=""))
        } else {
            jj <- as.character(seq(from=1,len=n))
            jj <- c(paste("x",jj,sep=""),paste("d",jj,sep=""))
        }
        options("sprayvars" = jj)
    }
    class(x) <- setdiff(class(x),"weyl")
    if(isTRUE(getOption("polyform",default=FALSE))){
        out <- print_spray_polyform(x)
    } else {
        out <- print_spray_matrixform(x)
    }
    return(weyl(out))
}

