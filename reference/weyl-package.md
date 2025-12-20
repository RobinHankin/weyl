# The Weyl Algebra

A suite of routines for Weyl algebras. Notation follows Coutinho (1995,
ISBN 0-521-55119-6, "A Primer of Algebraic D-Modules"). Uses 'disordR'
discipline (Hankin 2022 \<doi:10.48550/arXiv.2210.03856\>). To cite the
package in publications, use Hankin 2022
\<doi:10.48550/arXiv.2212.09230\>.

## Details

The DESCRIPTION file: This package was not yet installed at build
time.  
Index: This package was not yet installed at build time.  

## Author

Robin K. S. Hankin \[aut, cre\] (ORCID:
\<https://orcid.org/0000-0001-5982-0415\>)

Maintainer: Robin K. S. Hankin \<hankin.robin@gmail.com\>

## Examples

``` r
x <- rweyl(d=1)
y <- rweyl(d=1)
z <- rweyl(d=1)

is.zero(x*(y*z) - (x*y)*z)  # should be TRUE
#> [1] TRUE
```
