"Ops.weyl" <- function (e1, e2 = NULL) 
{
    unary <- nargs() == 1
    lclass <- nchar(.Method[1]) > 0
    rclass <- !unary && (nchar(.Method[2]) > 0)
    
    if(unary){
        class(e1) <- setdiff(class(e1),"weyl")
        out <- get(.Generic)(e1)
        } else if((.Generic == "*") & lclass & rclass){
            out <- weyl_prod_multivariate_nrow_allcolumns(e1,e2)
        } else if((.Generic == "^") & lclass & !rclass){
            out <- weyl_power_scalar(e1,e2)
        } else {
            class(e1) <- setdiff(class(e1),"weyl")
            class(e2) <- setdiff(class(e2),"weyl")
            out <- get(.Generic)(e1,e2)
        }
    if(is.spray(out)){out <- weyl(out)}
    return(out)
}
