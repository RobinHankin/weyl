`print.weyl` <- function(x,...){
    cat("A member of the Weyl algebra:\n")
    jj <- getOption("weylvars")   # typically c("x","y","z")
    if(!is.null(jj)){  
        options("sprayvars" = c(jj,paste("d",jj,sep="")))
    }
    class(x) <- setdiff(class(x),"weyl")
    if(isTRUE(getOption("polyform",default=FALSE))){
        out <- print_spray_polyform(x)
    } else {
        out <- print_spray_matrixform(x)
    }
    return(out)
}

