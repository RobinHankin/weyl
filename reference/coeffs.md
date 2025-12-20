# Manipulate the coefficients of a weyl object

Manipulate the coefficients of a weyl object. The coefficients are
`disord` objects.

## Usage

``` r
coeffs(S) <- value
```

## Arguments

- S:

  A weyl object

- value:

  Numeric

## Details

To access coefficients of a weyl object `S`, use `spray::coeffs(S)`
\[package idiom is `coeffs(S)`\]. Similarly to access the index matrix
use `index(S)`.

The replacement method is package-specific; use `coeffs(S) <-value`.

## Value

Extraction methods return a `disord` object (possibly `drop`ped);
replacement methods return a `weyl` object.

## Author

Robin K. S. Hankin

## Examples

``` r
(a <- rweyl(9))
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   1  1  1  0  1  0  =    9
#>   2  0  0  0  2  1  =    8
#>   1  2  0  0  1  1  =    5
#>   0  2  1  0  2  1  =    4
#>   0  1  0  1  1  2  =    3
#>   2  1  2  1  1  0  =    7
#>   1  0  1  2  2  2  =    6
#>   0  0  1  2  2  0  =    2
#>   2  2  2  1  2  1  =    1
coeffs(a)
#> A disord object with hash fd862492195be038e15ac4d61a4f25ac847c6c9e and elements
#> [1] 9 8 5 4 3 7 6 2 1
#> (in some order)
coeffs(a)[coeffs(a)<3] <- 100
a
#>   x  y  z dx dy dz     val
#>   2  2  2  1  2  1  =  100
#>   0  0  1  2  2  0  =  100
#>   1  0  1  2  2  2  =    6
#>   2  1  2  1  1  0  =    7
#>   0  1  0  1  1  2  =    3
#>   0  2  1  0  2  1  =    4
#>   1  2  0  0  1  1  =    5
#>   2  0  0  0  2  1  =    8
#>   1  1  1  0  1  0  =    9
```
