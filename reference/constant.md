# The constant term

The constant of a `weyl` object is the coefficient of the term with all
zeros.

## Usage

``` r
constant(x, drop = TRUE)
constant(x) <- value
```

## Arguments

- x:

  Object of class `weyl`

- drop:

  Boolean with default `TRUE` meaning to return the value of the
  coefficient, and `FALSE` meaning to return the corresponding Weyl
  object

- value:

  Constant value to replace existing one

## Value

Returns a numeric or weyl object

## Author

Robin K. S. Hankin

## Note

The `constant.weyl()` function is somewhat awkward because it has to
deal with the difficult case where the constant is zero and
`drop=FALSE`.

## Examples

``` r
(a <- rweyl() + 700)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   1  2  1  2  1  2  =    3
#>   0  0  0  1  1  2  =    2
#>   2  0  1  1  1  0  =    1
#>   0  0  0  0  0  0  =  700
constant(a)
#> [1] 700
constant(a, drop=FALSE)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  0  0  0  0  0  =  700

constant(a) <- 0
constant(a)
#> [1] 0
constant(a, drop=FALSE)
#> A member of the Weyl algebra:
#> empty sparse array with 6 columns

constant(a + 66) == constant(a) + 66
#> [1] TRUE
```
