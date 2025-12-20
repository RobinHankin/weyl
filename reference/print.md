# Print methods for weyl objects

Printing methods for weyl objects follow those for the
[spray](https://CRAN.R-project.org/package=spray) package, with some
additional functionality.

## Usage

``` r
# S3 method for class 'weyl'
print(x, ...)
```

## Arguments

- x:

  A weyl object

- ...:

  Further arguments, currently ignored

## Details

Option `polyform` determines whether the object is to be printed in
matrix form or polynomial form: as in the
[spray](https://CRAN.R-project.org/package=spray) package, this option
governs dispatch to either `print_spray_polyform()` or
`print_spray_matrixform()`.

    > a <- rweyl()
    > a    # default print method
    A member of the Weyl algebra:
      x  y  z dx dy dz     val
      1  2  2  2  1  0  =    3
      2  2  0  0  1  1  =    2
      0  0  0  1  1  2  =    1
    > options(polyform = TRUE)
    > a
    A member of the Weyl algebra:
    +3*x*y^2*z^2*dx^2*dy +2*x^2*y^2*dy*dz +dx*dy*dz^2
    > options(polyform = FALSE)  # restore default

Irrespective of the value of `polyform`, option `weylvars` controls the
variable names. If `NULL` (the default), then sensible values are used:
either `[xyz]` if the dimension is three or less, or integers. But
option `weylvars` is user-settable:

    > options(weylvars=letters[18:20])
    > a
    A member of the Weyl algebra:
      r  s  t dr ds dt     val
      1  2  2  2  1  0  =    3
      2  2  0  0  1  1  =    2
      0  0  0  1  1  2  =    1
    > options(polyform=TRUE)
    > a
    A member of the Weyl algebra:
    +3*r*s^2*t^2*dr^2*ds +2*r^2*s^2*ds*dt +dr*ds*dt^2
    > options(polyform=FALSE) ; options(weylvars=NULL)

If the user sets `weylvars`, the print method tries to do the Right
Thing (tm). If set to `c("a","b","c")`, for example, the generators are
named `c(" a"," b"," c","da","db","dc")` \[note the spaces\]. If the
algebra is univariate, the names will be something like `d` and `x`. No
checking is performed and if the length is not equal to the dimension,
undesirable behaviour may occur. For the love of God, do not use a
variable named `d`. Internally, `weylvars` works by changing the
`sprayvars` option in the
[spray](https://CRAN.R-project.org/package=spray) package.

Note that, as for `spray` objects, this option has no algebraic
significance: it only affects the print method.

## Value

Returns a `weyl` object.

## Author

Robin K. S. Hankin

## Examples

``` r
a <- rweyl()
print(a)
#> A member of the Weyl algebra:
#>   x  y  z dx dy dz     val
#>   0  0  0  1  0  1  =    3
#>   0  2  0  1  0  0  =    2
#>   2  0  1  1  2  1  =    1
options(polyform=TRUE)
print(a)
#> A member of the Weyl algebra:
#> +3*dx*dz +2*y^2*dx +x^2*z*dx*dy^2*dz
```
