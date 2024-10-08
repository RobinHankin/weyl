Weyl algebra in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/weyl.png" width = "150" align="right" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RobinHankin/weyl/branch/master/graph/badge.svg)](https://app.codecov.io/gh/RobinHankin/weyl?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/weyl?color=green)](https://cran.r-project.org/package=weyl)
[![Codecov test
coverage](https://codecov.io/gh/RobinHankin/weyl/graph/badge.svg)](https://app.codecov.io/gh/RobinHankin/weyl)
<!-- badges: end -->

To cite the `weyl` package in publications please use Hankin 2022. The
`weyl` package provides R-centric functionality for working with Weyl
algebras of arbitrary dimension. A detailed vignette is provided in the
package.

The Weyl algebra is a noncommutative algebra which is used in quantum
mechanics and the theory of differential equations (Coutinho 1997). The
`weyl` package offers a consistent and documented suite of R-centric
software. It is based on the `spray` package for sparse arrays for
computational efficiency.

The Weyl algebra is arguably the simplest noncommutative algebra and is
useful in quantum mechanics. It is isomorphic to the quotient ring of
the free algebra on two elements
![\left\lbrace X,Y\right\rbrace](https://latex.codecogs.com/png.latex?%5Cleft%5Clbrace%20X%2CY%5Cright%5Crbrace "\left\lbrace X,Y\right\rbrace")
over the ideal generated by
![XY=YX+1](https://latex.codecogs.com/png.latex?XY%3DYX%2B1 "XY=YX+1").
The `weyl` package implements this and also the
![n](https://latex.codecogs.com/png.latex?n "n")-th Weyl algebra.

One usually writes the Weyl algebra in terms of operators
![x,\partial](https://latex.codecogs.com/png.latex?x%2C%5Cpartial "x,\partial")
where ![x](https://latex.codecogs.com/png.latex?x "x") means multiply by
![x](https://latex.codecogs.com/png.latex?x "x") and
![\partial](https://latex.codecogs.com/png.latex?%5Cpartial "\partial")
means differentiate with respect to
![x](https://latex.codecogs.com/png.latex?x "x"). We find that
![\partial x-x\partial=1](https://latex.codecogs.com/png.latex?%5Cpartial%20x-x%5Cpartial%3D1 "\partial x-x\partial=1").

The Weyl algebra is also known as the symplectic Clifford algebra.

# Installation

You can install the released version of the weyl package from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("weyl")  # uncomment this to install the package
library("weyl")
set.seed(0)
```

# The `weyl` package in use

The basic creation function is `weyl()`, which takes a `spray` object
and returns a member of the Weyl algebra.

``` r
S <- spray(rbind(c(1,0,0,1,1,0),c(0,1,1,3,2,0)) ,1:2)
S
#>                  val
#>  0 1 1 3 2 0  =    2
#>  1 0 0 1 1 0  =    1
```

Above, object `S` is a standard `spray` object but to work with Weyl
algebra we need to coerce it to a `weyl` object with `weyl()`:

``` r
W <- weyl(S)
W
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  1  1  3  2  0  =    2
#>   1  0  0  1  1  0  =    1
```

Above, object `W` is a member of the third Weyl algebra: that is, the
algebra generated by ![\left\lbrace
x,y,z,\partial_x,\partial_y,\partial_z\right\rbrace](https://latex.codecogs.com/png.latex?%5Cleft%5Clbrace%0Ax%2Cy%2Cz%2C%5Cpartial_x%2C%5Cpartial_y%2C%5Cpartial_z%5Cright%5Crbrace "\left\lbrace
x,y,z,\partial_x,\partial_y,\partial_z\right\rbrace"). In this case
![W=x\partial_x\partial_y + 2yz\partial_x^3\partial_y^2](https://latex.codecogs.com/png.latex?W%3Dx%5Cpartial_x%5Cpartial_y%20%2B%202yz%5Cpartial_x%5E3%5Cpartial_y%5E2 "W=x\partial_x\partial_y + 2yz\partial_x^3\partial_y^2").
In other words ![Wf=x\frac{\partial^2f}{\partial x\partial y} +
2yz\frac{\partial^5f}{\partial x^3\partial y^2}](https://latex.codecogs.com/png.latex?Wf%3Dx%5Cfrac%7B%5Cpartial%5E2f%7D%7B%5Cpartial%20x%5Cpartial%20y%7D%20%2B%0A2yz%5Cfrac%7B%5Cpartial%5E5f%7D%7B%5Cpartial%20x%5E3%5Cpartial%20y%5E2%7D "Wf=x\frac{\partial^2f}{\partial x\partial y} +
2yz\frac{\partial^5f}{\partial x^3\partial y^2}").

We might ask what ![WWf](https://latex.codecogs.com/png.latex?WWf "WWf")
is, and this is easy in the package:

``` r
Wsquared <- W*W
Wsquared
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  2  2  6  4  0  =    4
#>   0  1  2  6  3  0  =    8
#>   0  1  1  3  3  0  =    6
#>   1  1  1  4  3  0  =    4
#>   2  0  0  2  2  0  =    1
#>   1  0  1  4  2  0  =    2
#>   1  0  0  1  2  0  =    1
```

This is a more complicated operator. However, we might wish to display
it in symbolic form:

``` r
options(polyform=TRUE)
Wsquared
#> A member of the Weyl algebra:
#> +4*y^2*z^2*dx^6*dy^4 +8*y*z^2*dx^6*dy^3 +6*y*z*dx^3*dy^3
#> +4*x*y*z*dx^4*dy^3 +x^2*dx^2*dy^2 +2*x*z*dx^4*dy^2 +x*dx*dy^2
```

## References

- S. C. Coutinho 1997. *The many avatars of a simple algebra*. The
  American Mathematical Monthly, 104(7):593-604. DOI
  <https://doi.org/10.1080/00029890.1997.11990687>.

- Hankin 2022. *Quantum algebra in R: the weyl package*. Arxiv, DOI
  <https://doi.org/10.48550/ARXIV.2212.09230>.

# Further information

For more detail, see the package vignette

`vignette("weyl")`
