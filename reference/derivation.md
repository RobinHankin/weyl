# Derivations

A derivation \\D\\ of an algebra \\A\\ is a linear operator that
satisfies \\D(d_1d_2)=d_1D(d_2)+D(d_1)d_2\\, for every \\d_1,d_2\in A\\.
If a derivation is of the form \\D(d)=\[d,f\]=df-fd\\ for some fixed
\\f\in A\\, we say that \\D\\ is an inner derivation.

Function `as.der()` returns a derivation with `as.der(f)(g)=fg-gf`.

## Usage

``` r
as.der(S)
```

## Arguments

- S:

  Weyl object

## Value

Returns a function, a derivation

## Author

Robin K. S. Hankin

## Examples

``` r
(o <- rweyl(n=2,d=2))
#> A member of the Weyl algebra:
#>   x  y dx dy     val
#>   0  1  0  0  =    2
#>   1  2  1  1  =    1
(f <- as.der(o))
#> function (x) 
#> {
#>     S * x - x * S
#> }
#> <bytecode: 0x55c91985f250>
#> <environment: 0x55c91985ee60>

d1 <-rweyl(n=1,d=2)
d2 <-rweyl(n=2,d=2)

f(d1*d2) == d1*f(d2) + f(d1)*d2 # should be TRUE
#> [1] TRUE
```
