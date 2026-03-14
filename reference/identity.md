# The identity operator

The identity operator maps any function to itself.

## Usage

``` r
idweyl(d)
# S3 method for class 'weyl'
as.id(S)
is.id(S)
```

## Arguments

- d:

  Integer specifying dimensionality of the weyl object (twice the spray
  arity)

- S:

  A weyl object

## Value

A weyl object corresponding to the identity operator

## Note

The identity function cannot be called “`id()`” because then R would not
know whether to create a `spray` or a `weyl` object.

## Examples

``` r
idweyl(7)
#> A member of the Weyl algebra:
#>   1  2  3  4  5  6  7 d1 d2 d3 d4 d5 d6 d7     val
#>   0  0  0  0  0  0  0  0  0  0  0  0  0  0  =    1

a <- rweyl(d=5)
a
#> A member of the Weyl algebra:
#>   1  2  3  4  5 d1 d2 d3 d4 d5     val
#>   0  2  1  0  0  1  0  1  1  2  =    3
#>   1  1  1  2  0  1  1  0  2  0  =    2
#>   2  2  2  2  0  2  2  0  0  0  =    1
is.id(a)
#> [1] FALSE
is.id(1+a-a)
#> [1] TRUE
as.id(a)
#> A member of the Weyl algebra:
#>   1  2  3  4  5 d1 d2 d3 d4 d5     val
#>   0  0  0  0  0  0  0  0  0  0  =    1

a == a*1
#> [1] TRUE
a == a*as.id(a)
#> [1] TRUE
```
