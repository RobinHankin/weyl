\name{print.weyl}
\alias{print.weyl}
\alias{print}
\title{Print methods for weyl objects}

\description{

Printing methods for weyl objects follow those for the \pkg{spray}
package, with some additional functionality.

}
\usage{\method{print}{weyl}(x, ...)}
\arguments{
  \item{x}{A weyl object}
  \item{\dots}{Further arguments, currently ignored}
}
\details{
Option \code{polyform} determines whether the object is to be printed in
matrix form or polynomial form: as in the \pkg{spray} package, this
option governs dispatch to either \code{print_spray_polyform()} or
\code{print_spray_matrixform()}.


\preformatted{
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
}

Irrespective of the value of \code{polyform}, option \code{weylvars}
controls the variable names.  If \code{NULL} (the default), then
sensible values are used: either \code{[xyz]} if the dimension is three
or less, or integers.  But option \code{weylvars} is user-settable:

\preformatted{
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
}

If the user sets \code{weylvars}, the print method tries to do the Right
Thing (tm).  If set to \code{c("a","b","c")}, for example, the
generators are named \code{c(" a"," b"," c","da","db","dc")} [note the
spaces].  If the algebra is univariate, the names will be something like
\code{d} and \code{x}.  No checking is performed and if the length is
not equal to the dimension, undesirable behaviour may occur.  For the
love of God, do not use a variable named \code{d}.  Internally,
\code{weylvars} works by changing the \code{sprayvars} option in the
\pkg{spray} package.

Note that, as for the \pkg{spray} package, this option has no algebraic
significance: it only affects the print method.

}

\value{Returns a \code{weyl} object.}
\author{Robin K. S. Hankin}
\examples{
a <- rweyl()
print(a)
options(polyform=TRUE)
print(a)
}