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
#>   1  2  0  0  1  1  =    9
#>   0  2  1  0  2  1  =    8
#>   0  1  0  1  1  2  =    7
#>   0  0  1  2  2  0  =    6
#>   2  2  2  1  2  1  =    5
#>   2  2  0  0  0  2  =    3
#>   1  1  1  1  0  1  =    4
#>   2  2  1  2  1  1  =    2
#>   1  1  0  1  2  2  =    1
coeffs(a)
#> A disord object with hash b718add1b991c5338effd09f1b672587009a4c6b and elements
#> [1] 9 8 7 6 5 3 4 2 1
#> (in some order)
coeffs(a)[coeffs(a)<3] <- 100
a
#>   x  y  z dx dy dz     val
#>   1  1  0  1  2  2  =  100
#>   2  2  1  2  1  1  =  100
#>   1  1  1  1  0  1  =    4
#>   2  2  0  0  0  2  =    3
#>   2  2  2  1  2  1  =    5
#>   0  0  1  2  2  0  =    6
#>   0  1  0  1  1  2  =    7
#>   0  2  1  0  2  1  =    8
#>   1  2  0  0  1  1  =    9
```
