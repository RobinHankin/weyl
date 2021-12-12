test_that("Test suite aac.R",{  # as.der()

    checker3 <- function(x,y,z){
        fx <- as.der(x)
        expect_true(fx(y*z) == y*fx(z) + fx(y)*z)
    }

    for(i in seq_len(1)){
        x <- rweyl(n=2,d=2)
        y <- rweyl(n=2,d=2)
        z <- rweyl(n=2,d=2)
        checker3(x,y,z)
    }
})
        
