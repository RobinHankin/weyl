# Create spray objects

Function `spray()` creates a sparse array; function
[`weyl()`](https://robinhankin.github.io/weyl/reference/weyl.md) coerces
a spray object to a Weyl object.

## Usage

``` r
spray(M, x, addrepeats=FALSE)
```

## Arguments

- M:

  An integer-valued matrix, the index of the weyl object

- x:

  Numeric vector of coefficients

- addrepeats:

  Boolean, specifying whether repeated rows are to be added

## Details

The function is discussed and motivated in the spray package.

## Value

Return a weyl or a Boolean

## Author

Robin K. S. Hankin

## Examples

``` r
(W <- spray(matrix(1:36,6,6), 1:6))
#>  x  d <NA> <NA> <NA> <NA>     val
#>  6 12   18   24   30   36  =    6
#>  3  9   15   21   27   33  =    3
#>  5 11   17   23   29   35  =    5
#>  4 10   16   22   28   34  =    4
#>  2  8   14   20   26   32  =    2
#>  1  7   13   19   25   31  =    1
weyl(W)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   6 12 18 24 30 36  =    6
#>   3  9 15 21 27 33  =    3
#>   5 11 17 23 29 35  =    5
#>   4 10 16 22 28 34  =    4
#>   2  8 14 20 26 32  =    2
#>   1  7 13 19 25 31  =    1

as.weyl(15, d=3)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  0  0  0  0  0  =   15
```
