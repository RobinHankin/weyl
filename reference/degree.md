# The degree of a `weyl` object

The degree of a monomial weyl object \\x^a\partial^b\\ is defined as
\\a+b\\. The degree of a general weyl object expressed as a linear
combination of monomials is the maximum of the degrees of these
monomials. Following Coutinho we have:

- \\\mathrm{deg}(d_1+d_2)\leq\max(\mathrm{deg}(d_1)+
  \mathrm{deg}(d_2))\\

- \\\mathrm{deg}(d_1d_2) = \mathrm{deg}(d_1)+ \mathrm{deg}(d_2)\\

- \\\mathrm{deg}(d_1d_2-d_2d_1)\leq\mathrm{deg}(d_1)+
  \mathrm{deg}(d_2)-2\\

## Usage

``` r
deg(S)
```

## Arguments

- S:

  Object of class `weyl`

## Value

Nonnegative integer (or \\-\infty\\ for the zero Weyl object)

## Author

Robin K. S. Hankin

## Note

The degree of the zero object is conventionally \\-\infty\\.

## Examples

``` r
(a <- rweyl())
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  0  1  2  2  2  =    3
#>   0  1  0  2  2  0  =    2
#>   0  0  0  0  0  1  =    1
deg(a)
#> [1] 7

d1 <- rweyl(n=2)
d2 <- rweyl(n=2)

deg(d1+d2) <= deg(d1) + deg(d2)
#> [1] TRUE
deg(d1*d2) == deg(d1) + deg(d2)
#> [1] TRUE
deg(d1*d2 - d2*d1) <= deg(d1) + deg(d2) -2
#> [1] TRUE
```
