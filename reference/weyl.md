# The algebra and weyl objects

Basic functions for weyl objects

## Usage

``` r
weyl(M)
is.weyl(M)
as.weyl(val,d)
is.ok.weyl(M)
```

## Arguments

- M:

  A weyl or spray object

- val,d:

  Value and dimension for weyl object

## Details

To create a weyl object, pass a spray to function `weyl()`, as in
`weyl(M)`. To create a spray object to pass to `weyl()`, use function
[`spray()`](https://robinhankin.github.io/weyl/reference/spray.md),
which is a synonym for
[`spray::spray()`](https://robinhankin.github.io/spray/reference/spray.html).

Function `weyl()` is the formal creator method; `is.weyl()` tests for
weyl objects and `is.ok.weyl()` checks for well-formed sprays. Function
`as.weyl()` tries (but not very hard) to infer what the user intended
and return the right thing.

## Value

Return a weyl or a Boolean

## Author

Robin K. S. Hankin

## Examples

``` r
(W <- spray(matrix(1:36,6,6),1:6))
#>   x  y  z dx dy dz     val
#>   6 12 18 24 30 36  =    6
#>   3  9 15 21 27 33  =    3
#>   5 11 17 23 29 35  =    5
#>   4 10 16 22 28 34  =    4
#>   2  8 14 20 26 32  =    2
#>   1  7 13 19 25 31  =    1
weyl(W)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   6 12 18 24 30 36  =    6
#>   3  9 15 21 27 33  =    3
#>   5 11 17 23 29 35  =    5
#>   4 10 16 22 28 34  =    4
#>   2  8 14 20 26 32  =    2
#>   1  7 13 19 25 31  =    1

as.weyl(15,d=3)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  0  0  0  0  0  =   15

is.ok.weyl(spray(matrix(1:30,5,6)))
#> [1] TRUE
if (FALSE) is.ok.weyl(spray(matrix(1:30,6,5))) # \dontrun{}
```
