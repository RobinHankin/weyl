test_that("Test suite aaa.R",{
    expect_silent(as.weyl(10,4))
    expect_error(as.weyl(10))
    expect_error(as.weyl("i"))
    expect_true(is.weyl(as.weyl(weyl(spray(matrix(1:20,5,4))))))

    expect_error(weyl(matrix(1:10,2,5)))
    expect_silent(weyl(matrix(1:8,2,4)))
    expect_silent(as.weyl(matrix(1:8,2,4)))
    expect_silent(as.weyl(matrix(1:8,2,4),1:2))

    a <- weyl(spray(diag(1:6),1:6))
    coeffs(a) <- 1

    ## next line reveals a bug in the package
    expect_error(coeffs(a) <- disordR::disord(c(1,2,3,4,6,5)))

    expect_true(a == weyl(spray(diag(1:6))))
    expect_error(coeffs(a) <- 1:2)
    expect_error(coeffs(a) <- 1:6)
    expect_true(constant(a) == 0)
    
    constant(a) <- 19
    expect_true(constant(a) == 19)
    expect_true(constant(constant(a,drop=FALSE),drop=TRUE) == 19)

    expect_true(constant(idweyl(8)) == 1)
    

    expect_true(constant(as.id(rweyl())) == 1)
    expect_true(is.id(as.id(rweyl())))

    expect_true(dim(rweyl(d=9)) == 9)
    expect_true(deg(weyl(spray(diag(1:8)))) == 8)
 
    expect_true(is.zero(zero(9)))


    expect_true(drop(zero(9)) == 0)
    expect_true(drop(as.weyl(7,4)) == 7)

    a <- weyl(spray(diag(1:8),1:8))
    expect_true(a == drop(a))

    a <- spray(matrix(1:3,3,5,byrow=TRUE),1:3)
    expect_true(all(grades(a) %in% 9:11))
    expect_true(length(grades(a*0)) == 0)

    a <- weyl(
        spray(matrix(floor((1.1)^(1:42))%%3,7,6,byrow=TRUE),1:7,addrepeats=TRUE)
    )

    expect_true(all(grades(grade(a,5)) == 5))
    expect_true(is.zero(grade(a,7)))
    grade(a,5) <- 0
    expect_true(all(grades(a) != 5))

    a <- weyl(spray(matrix(1:10,ncol=2)))
    b <- weyl(spray(matrix(1:8,ncol=2)))
    
    expect_true(weyl(weyl_prod_univariate_nrow(a,b)) == a*b)

    expect_true((d*x)^2 == (d*x)*(d*x))


    expect_true(d^5 == weyl(spray(cbind(0,5))))
    expect_true(x^7 == weyl(spray(cbind(7,0))))
    
    options("prodfunc" = NULL)
    a0 <- a*b
    options("prodfunc" = weyl_prod_helper1)
    a1 <- a*b
    options("prodfunc" = weyl_prod_helper2)
    a2 <- a*b

    expect_true(a0==a1)
    expect_true(a1==a2)
    
    options("prodfunc" = NULL)

    options("weylvars" = NULL)
    expect_output(print(weyl(spray(matrix(1:16,ncol=8)))))


    
})

