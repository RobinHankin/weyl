## This file follows the structure of aaa.R in the free group package.

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.  Equation numbers are from Hestenes.


test_that("Test suite aaa.R",{

checker1 <- function(A){

  expect_true(A == +A)
  expect_true(A == -(-A))
  expect_error(!A)

  expect_true(A == A+0) 
  expect_false(A == 1+A)
  expect_false(1+A == A)
  expect_false(A == A+1)
    
  expect_true(A+A == 2*A)
  expect_true(A+A == A*2)

  expect_true(is.zero(A-A))
  expect_true(A+A+A == 3*A)
  expect_true(A+A+A == A*3)

  expect_true(A/2 + A/2 == A)

  expect_error(A&A)
  expect_true(A*A == A^2)

  expect_true(A^0 ==     1)
  expect_true(A^1 ==     A)
  expect_true(A^2 ==   A*A)
  expect_true(A^3 == A*A*A)
  
  expect_error(A^-1)


  expect_output(print(A))
  options("polyform" = FALSE)
  expect_output(print(A))
  options("polyform" = TRUE)
  expect_output(print(A))

  options("weylvars" = letters[seq_len(dim(A))])
  options("polyform" = FALSE)
  expect_output(print(A))
  options("polyform" = TRUE)
  expect_output(print(A))
    
  
}   # checker1() closes
  
checker2 <- function(A,B){
  expect_true(A+B == B+A) # 1.1
  expect_true(A+2*B == B+B+A)

}   # checker2() closes

checker3 <- function(A,B,C){
  expect_true(A+(B+C) == (A+B)+C)  # addition is associative
  expect_true(A*(B*C) == (A*B)*C)  # product is associative


  expect_true(A*(B+C) == A*B + A*C) # left distributive
  expect_true((A+B)*C == A*C + B*C) # right distributive

}
  
for(i in seq_len(3)){

    A <- rweyl(n=2,dim=3)
    B <- rweyl(n=2,dim=3)
    C <- rweyl(n=2,dim=3)

    cat(paste("Checking test", i, "...\n"))
    checker1(A)
    checker2(A,B)
    checker3(A,B,C)
    cat(paste("Test ",i, " passed\n"))

}
A1 <- rweyl(n=2,dim=1)
checker1(A1)
})
