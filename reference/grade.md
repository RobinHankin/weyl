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
#> A disord object with hash d3e95ebc6f51951331de5094fe478d929ba9bdcd and elements
#>  [1] 5 8 7 5 4 4 6 6 8 8 4 6 5 3 6 4 6 8 7 5 7 5 8 8 7 7 4 6 6 5
#> (in some order)
grade(a, 1:4)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   1  0  2  0  0  1  =   25
#>   1  0  0  0  1  1  =   24
#>   2  0  0  1  1  0  =    6
#>   2  1  1  0  0  0  =    7
#>   1  0  0  0  2  1  =   22
#>   0  1  1  1  1  0  =   11
grade(a, 5:9) <- -99
a
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   1  0  1  1  2  1  =  -99
#>   2  1  1  2  1  1  =  -99
#>   2  1  1  0  0  0  =    7
#>   0  1  2  2  1  2  =  -99
#>   2  2  0  1  0  0  =  -99
#>   1  0  0  0  1  1  =   24
#>   2  0  0  2  0  1  =  -99
#>   2  0  2  0  1  2  =  -99
#>   1  2  2  1  2  0  =  -99
#>   1  0  0  0  2  1  =   22
#>   1  1  1  0  2  1  =  -99
#>   1  2  1  1  1  0  =  -99
#>   0  1  1  1  1  0  =   11
#>   0  2  0  1  0  2  =  -99
#>   0  1  2  2  1  0  =  -99
#>   2  0  0  1  1  0  =    6
#>   2  2  0  1  1  0  =  -99
#>   2  2  1  1  1  1  =  -99
#>   0  2  1  2  2  0  =  -99
#>   0  0  2  2  1  0  =  -99
#>   0  2  1  1  1  2  =  -99
#>   1  1  0  2  1  0  =  -99
#>   2  2  2  0  2  0  =  -99
#>   1  1  1  0  1  1  =  -99
#>   2  0  0  2  2  0  =  -99
#>   0  1  1  1  2  1  =  -99
#>   2  2  2  0  0  2  =  -99
#>   1  0  1  2  1  2  =  -99
#>   1  2  0  2  1  1  =  -99
#>   1  0  2  0  0  1  =   25
```
