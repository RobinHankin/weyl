# Random weyl objects

Creates random weyl objects: quick-and-dirty examples of Weyl algebra
elements

## Usage

``` r
rweyl(nterms = 3, vals = seq_len(nterms), dim = 3, powers = 0:2)
rweyll(nterms = 15, vals = seq_len(nterms), dim = 4, powers = 0:5)
rweylll(nterms = 50, vals = seq_len(nterms), dim = 8, powers = 0:7)
```

## Arguments

- nterms:

  Number of terms in output

- vals:

  Values of coefficients

- dim:

  Dimension of weyl object

- powers:

  Set from which to sample the entries of the index matrix

## Details

Function `rweyl()` creates a smallish random Weyl object; `rweyll()` and
`rweylll()` create successively more complicated objects.

These functions use
[`spray::rspray()`](https://robinhankin.github.io/spray/reference/rspray.html),
so the note there about repeated rows in the index matrix resulting in
fewer than `nterms` terms applies.

Function `rweyl1()` returns a one-dimensional Weyl object.

## Value

Returns a weyl object

## Author

Robin K. S. Hankin

## Examples

``` r
rweyl()
#> A member of the Weyl algebra:
#> +3*x*y^2*dx*dz +2*x*y^2*dy*dz +z^2*dx^2*dy
rweyll()
#> A member of the Weyl algebra:
#> +15*x1^4*x2^3*x3^4*x4*d1*d2^3*d3^3*d4
#> +14*x1^5*x2^3*x3^5*x4^4*d1^5*d2*d3^2*d4^5
#> +9*x1^2*x2^2*x3^4*x4^3*d1^2*d2^2*d3^5 +x2*x4^3*d1*d2^4*d3^4*d4^4
#> +2*x1^3*x2^3*x3*x4^5*d1^2*d2^2*d3^4*d4^2
#> +4*x1^2*x2^4*x3^4*x4^2*d1*d2*d3^4 +3*x2*x3^3*x4^5*d1^4*d2^4*d3^4
#> +5*x1^4*x2^3*x3^5*x4^2*d1^4*d3^2*d4^2 +6*x1^4*x2*x3^4*d1^2
#> +7*x2*x3^4*x4^3*d1^3*d3*d4^3 +12*x2*x3^3*d2^2*d3^2*d4^5
#> +8*x1*x2*x3^5*x4*d1^5*d2*d3^5*d4^2 +10*x1*x2^3*x3^4*x4^3*d1^3*d2^4*d3^5
#> +11*x2*x3^3*x4^5*d2^2*d4^3 +13*x1^4*x2^2*x3^3*x4^3*d1^4*d2^5*d3*d4^4
rweyl(d=7)
#> A member of the Weyl algebra:
#> +3*x1^2*x3*x4^2*x5^2*x6*x7*d1^2*d2^2*d3*d4*d6^2
#> +2*x2^2*x3^2*x4^2*x5*x6*x7^2*d1*d2*d3*d7
#> +x1*x2^2*x3^2*x4^2*x5^2*x6^2*d1^2*d3^2*d4^2*d6^2*d7^2

options(polyform = TRUE)
rweyl1()
#> A member of the Weyl algebra:
#> +5*x*d^4 +4*x^4*d^4 +2*x^3*d +6*x^3*d^2 +4*x^5*d^2
options(polyform = FALSE) # restore default
```
