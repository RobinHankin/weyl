`print.weyl` <- function(x,...){
    jj <- getOption("weylvars")   # typically c("x","y","z")
    if(!is.null(jj)){  
        options("sprayvars" = c(jj,paste("d",jj,sep="")))
    }
    if(isTRUE(getOption("polyform",default=FALSE))){
        out <- print_spray_polyform(x)
    } else {
        out <- print_spray_matrixform(x)
    }
    return(out)
}

