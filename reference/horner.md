# Horner's method

Horner's method

## Usage

``` r
horner(W,v)
```

## Arguments

- W:

  Weyl object

- v:

  Numeric vector of coefficients

## Details

Given a formal polynomial

\$\$p(x) = a_0 +a_1+a_2x^2+\cdots + a_nx^n\$\$

it is possible to express \\p(x)\\ in the algebraically equivalent form

\$\$p(x) = a_0 + x\left(a_1+x\left(a_2+\cdots + x\left(a\_{n-1} +xa_n
\right)\cdots\right)\right)\$\$

which is much more efficient for evaluation, as it requires only \\n\\
multiplications and \\n\\ additions, and this is optimal.

## Author

Robin K. S. Hankin

## See also

[`ooom`](https://robinhankin.github.io/weyl/reference/ooom.md)

## Examples

``` r
horner(x, 1:5)
#> A member of the Weyl algebra:
#>  x d     val
#>  4 0  =    5
#>  3 0  =    4
#>  2 0  =    3
#>  0 0  =    1
#>  1 0  =    2
horner(x+d, 1:3)
#> A member of the Weyl algebra:
#>  x d     val
#>  2 0  =    3
#>  0 0  =    4
#>  0 1  =    2
#>  1 1  =    6
#>  0 2  =    3
#>  1 0  =    2

2+x+d |> horner(1:3) |> horner(1:2)
#> A member of the Weyl algebra:
#>  x d     val
#>  1 0  =    1
#>  0 0  =    5
#>  0 1  =    4
#>  0 2  =    6
```
