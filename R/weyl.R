#' @importFrom disordR elements
#' @importFrom disordR disord
#' @importFrom disordR hash
#' @importFrom disordR hashcal
#' @importFrom disordR elements
#' @importFrom disordR is.disord
#' @importFrom disordR consistent
#' @importFrom disordR %in%

#' @importClassesFrom freealg dot
#' @importMethodsFrom freealg "["

#' @importFrom spray coeffs
#' @importFrom spray coeffs<-
#' @importFrom spray is.spray
#' @importFrom spray arity
#' @importFrom spray print_spray_matrixform
#' @importFrom spray index
#' @importFrom spray nterms

#' @importMethodsFrom spray coeffs<-


#' @importFrom methods setOldClass


#' @export
`weyl` <- function(M){
    if(is.matrix(M)){M <- spray(M)}
    stopifnot(is.ok.weyl(M))
    return(structure(M, class = c("weyl","spray"))) # class weyl set only here
}

setOldClass("weyl")

#' @export
`is.ok.weyl` <- function(M){
    if(!is.spray(M)){stop("need a spray")}
    if(arity(M)%%2 != 0){stop("arity must be even")}
    return(TRUE)
}
    
#' @export
`spray` <- function (M, x, addrepeats = FALSE){spray::spray(M,x,addrepeats=addrepeats)}
                    
#' @export
`is.weyl` <- function(M){inherits(M, "weyl")}

#' @export
`as.weyl` <- function(val, d){
    if(is.weyl(val) | is.spray(val)){
        out <- val
    } else if(is.matrix(val)){
        out <- spray(val, d)
    } else if(is.numeric(val)){
        return(val*idweyl(d))
    } else {
        stop("not recognised")
    }
    return(weyl(out))
}

#' @export
`rweyl` <- function(nterms = 3, vals = seq_len(nterms), dim = 3, powers = 0:2){
    weyl(rspray(n = nterms, vals = vals, arity = dim*2, powers = powers))
}

#' @export
`rweyll` <- function(nterms = 15, vals = seq_len(nterms), dim = 4, powers = 0:5){
    rweyl(nterms = nterms, vals = vals, dim = dim, powers = powers)
}

#' @export
`rweylll` <- function(nterms = 50, vals = seq_len(nterms), dim = 8, powers = 0:7){
    rweyl(nterms = nterms, vals = vals, dim = dim, powers = powers)
}

#' @export
`rweyl1` <- function(nterms = 6, vals = seq_len(nterms), dim = 1, powers = 1:5){
    rweyl(nterms = nterms, vals = vals, dim = dim, powers = powers)
}


#' @export
`coeffs<-` <- function(S, value){UseMethod("coeffs<-")}

#' @export
setGeneric("coeffs<-")


#' @export
`coeffs<-.weyl` <- function(S, value){
    jj <- coeffs(S)
    if(is.disord(value)){
        stopifnot(consistent(coeffs(S), value))
        if((!identical(hash(jj), hash(value))) & (length(value) > 1)){stop("length > 1")}
        jj <- value
    } else {
        stopifnot(length(value) == 1)
        jj[] <- value  # the meat
    }
    return(weyl(spray(index(S), elements(jj))))
}

#' @export
`constant` <- function(x, drop = TRUE){UseMethod("constant")}

#' @export
`constant.weyl` <- function(x, drop = TRUE){
    class(x) <- setdiff(class(x),"weyl")
    out <- constant(x, drop = TRUE)
    if(drop){
        return(out)
    } else { # not dropped
        return(weyl(spraymaker(spray(matrix(0, 1, arity(x)), out), arity = arity(x))))
    }
}

#' @export
`constant<-` <- function(x, value){UseMethod("constant<-")}

#' @export
`constant<-.weyl` <- function(x, value){
    class(x) <- setdiff(class(x), "weyl")
    constant(x) <- value
    return(weyl(x))
}

#' @export
`idweyl` <- function(d){weyl(spray(matrix(0, 1, d*2), 1))}

#' @export
as.id <- function(x){UseMethod("as.id")}

#' @export
`as.id.weyl` <- function(S){idweyl(dim(S))}

#' @export
`is.id` <- function(S){S == as.id.weyl(S)}

#' @export
dim <- function(x){UseMethod("dim")}

#' @export
setGeneric("dim")

#' @export
`dim.weyl` <- function(x){arity(x)/2}

#' @export
setGeneric("drop")

#' @export
setMethod("drop","weyl", function(x){
    if(is.zero(x)){
        return(0)
    } else if(is.constant(x)){
        return(constant(x, drop = TRUE))
    } else {
        return(x)
    }
})

#' @export
`deg` <- function(S){max(c(-Inf, rowSums(index(S))))} # following Coutinho

#' @export
`zero` <- function(d){weyl(spray(matrix(0, 0, 2*d), numeric(0)))}

#' @export
`as.der` <- function(S){function(x){S*x - x*S}}

#' @export
setGeneric("sort")

#' @export
`is.zero` <- function(x){spray::is.zero(x)}

#' @export
`horner` <- function(W, v) {
    W <- as.weyl(W)
    Reduce(v, right = TRUE, f = function(a, b){b*W + a})
}

#' @export
`ooom` <- function(W, n){
  stopifnot(constant(W) == 0)
  stopifnot(n >= 0)
  if(n == 0){
    return(as.id(W))
  } else {
    return(horner(W, rep(1, n+1)))
  }
}
