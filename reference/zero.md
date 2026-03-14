# The zero operator

The zero operator maps any function to the zero function (which maps
anything to zero). To test for something being zero, use
[`spray::is.zero()`](https://robinhankin.github.io/spray/reference/zero.html);
package idiom would be `is.zero()`.

Function `zero()` takes a single argument which is interpreted as the
dimension of the result. So `zero(dim(a))` returns `a*0` (but is
faster).

## Usage

``` r
zero(d)
```

## Arguments

- d:

  Integer specifying dimensionality of the weyl object (twice the spray
  arity)

## Value

A weyl object corresponding to the zero operator (or a Boolean for
`is.zero()`)

## Examples

``` r
(a <- rweyl(d=5))
#> A member of the Weyl algebra:
#>   1  2  3  4  5 d1 d2 d3 d4 d5     val
#>   0  1  1  1  2  1  0  1  1  1  =    3
#>   1  2  2  0  1  0  1  0  0  0  =    2
#>   1  2  2  1  2  2  1  2  2  1  =    1
is.zero(a)
#> [1] FALSE
is.zero(a-a)
#> [1] TRUE
is.zero(a*0)
#> [1] TRUE

a == a + zero(dim(a))
#> [1] TRUE

zero(8)
#> A member of the Weyl algebra:
#> empty sparse array with 16 columns
```
