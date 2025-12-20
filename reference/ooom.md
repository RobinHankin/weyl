# One over one minus

Uses Taylor's theorem to give one over one minus a Weyl object

## Usage

``` r
ooom(W, n)
```

## Arguments

- W:

  Weyl object

- n:

  Order of expansion

## Author

Robin K. S. Hankin

## See also

[`horner`](https://robinhankin.github.io/weyl/reference/horner.md)

## Examples

``` r
ooom(x+d, 4)
#> A member of the Weyl algebra:
#>  x d     val
#>  2 1  =    3
#>  2 2  =    6
#>  0 4  =    1
#>  1 1  =   14
#>  4 0  =    1
#>  3 0  =    1
#>  0 3  =    1
#>  1 0  =    4
#>  1 3  =    4
#>  3 1  =    4
#>  1 2  =    3
#>  0 0  =    5
#>  0 1  =    4
#>  2 0  =    7
#>  0 2  =    7

W <- x + x*d
ooom(W, 4)*(1-W) == 1-W^5
#> [1] TRUE
```
