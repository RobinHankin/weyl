# The grade of a weyl object

The grade of a homogeneous term of a Weyl algebra is the sum of the
powers. Thus the grade of \\4xy^2\partial_x^3\partial_y^4\\ is
\\1+2+3+4=10\\.

The functionality documented here closely follows the equivalent in the
[clifford](https://CRAN.R-project.org/package=clifford) package.

Coutinho calls this the symbol map.

## Usage

``` r
grade(C, n, drop=TRUE)
grade(C, n) <- value
grades(x)
```

## Arguments

- C, x:

  Weyl object

- n:

  Integer vector specifying grades to extract

- value:

  Replacement value, a numeric vector

- drop:

  Boolean, with default `TRUE` meaning to coerce a constant operator to
  numeric, and `FALSE` meaning not to

## Details

Function `grades()` returns an (unordered) vector specifying the grades
of the constituent terms.

Function `grade(x,n)` returns a Weyl object with just the elements of
grade `g`, where `g %in% n`.

Function `grade<-()` allows idiom such as `grade(x,1:2) <- 7` to operate
as expected \[here to set all coefficients of terms with grades 1 or 2
to value 7\].

The zero grade term, `grade(x,0)`, is given more naturally by
`constant(C)`.

## Value

Integer vector or weyl object

## Author

Robin K. S. Hankin

## Examples

``` r
a <- rweyl(30)

grades(a)
#> A disord object with hash 855dc61a835ab43c9cf74e9cf606df82d3ec4cda and elements
#>  [1] 4 7 7 3 7 5 9 8 4 6 8 5 4 6 4 6 4 8 7 6 6 4 7 8 4 7 6 5 7 7
#> (in some order)
grade(a, 1:4)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   1  0  2  0  0  1  =   10
#>   1  1  0  0  0  2  =   22
#>   2  0  1  0  0  1  =   20
#>   1  0  0  0  2  1  =    7
#>   0  0  2  2  0  0  =   18
#>   1  0  0  0  1  1  =    9
#>   0  0  1  1  0  2  =   21
#>   2  0  1  1  0  0  =   30
grade(a, 5:9) <- -99
a
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   2  2  1  2  0  0  =  -99
#>   1  1  0  2  1  0  =  -99
#>   2  2  2  0  2  0  =  -99
#>   2  1  1  1  1  0  =  -99
#>   2  2  2  0  0  2  =  -99
#>   0  0  2  2  0  0  =   18
#>   1  0  1  2  1  2  =  -99
#>   1  2  2  1  2  1  =  -99
#>   2  0  0  2  0  1  =  -99
#>   2  0  2  0  1  2  =  -99
#>   1  0  0  0  1  1  =    9
#>   0  2  1  1  1  2  =  -99
#>   1  0  0  0  2  1  =    7
#>   2  0  1  1  0  0  =   30
#>   1  0  1  1  2  1  =  -99
#>   2  0  1  0  0  1  =   20
#>   0  2  2  1  0  1  =  -99
#>   1  1  0  0  0  2  =   22
#>   1  2  2  1  0  2  =  -99
#>   1  1  2  1  1  1  =  -99
#>   1  2  1  1  1  0  =  -99
#>   1  1  1  0  2  1  =  -99
#>   1  0  2  0  0  1  =   10
#>   1  2  0  2  1  1  =  -99
#>   1  1  1  2  1  2  =  -99
#>   0  0  1  1  0  2  =   21
#>   2  0  1  0  2  2  =  -99
#>   1  1  1  1  0  2  =  -99
#>   1  1  1  0  1  1  =  -99
#>   2  1  2  2  0  0  =  -99
```
