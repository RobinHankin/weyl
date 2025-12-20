# Drop redundant information

Coerce constant weyl objects to numeric

## Usage

``` r
drop(x)
```

## Arguments

- x:

  Weyl object

## Details

If its argument is a constant weyl object, coerce to numeric.

## Author

Robin K. S. Hankin

## Value

Returns either a length-one numeric vector or its argument, a weyl
object

## Note

Many functions in the package take `drop` as an argument which, if
`TRUE`, means that the function returns a `drop`ped value.

## Examples

``` r
a <- rweyl() + 67
drop(a)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  1  2  2  0  0  =    2
#>   0  0  0  0  0  0  =   67
#>   0  0  0  2  0  1  =    3
#>   1  1  0  2  2  0  =    1

drop(idweyl(9))
#> [1] 1

drop(constant(a, drop=FALSE))
#> [1] 67
```
